---
name: fftiers-ops
description: Operate the borischen.co fftiers pipeline — season rollover, run main.R, S3 deploy from Workbench or EC2. Use when updating the NFL year, running tiers, pushing to s3://fftiers, FantasyPros downloads, or Workbench/AWS setup for this repo.
---

# fftiers Ops

Fantasy football tier charts for [borischen.co](https://www.borischen.co). Data from FantasyPros API; outputs go to `out/current/` then `s3://fftiers/out/`.

## Repo layout

| Path | Purpose |
|------|---------|
| `src/config.R` | **Yearly config only:** `year`, `weekonetuesday` |
| `src/main.R` | Entry point: download + chart generation |
| `src/ff-functions.R` | Download helpers, clustering, ggplot output |
| `src/fp_api.py` | FantasyPros API → JSON/CSV (`src/api_key.py` required, **never commit**) |
| `src/push-to-s3.py` | Upload `out/current/{png,csv,txt}/` → `s3://fftiers/out/` |
| `src/master.py` | Cron wrapper: `main.R` then `push-to-s3.py` |
| `src/cron-run.sh` | Date-aware cron gate → `master.py` |
| `src/crontab.example` | Workbench crontab template |
| `dat/{year}/` | Raw downloaded data (gitignored) |
| `out/current/{png,csv,txt}/` | Website-facing outputs (gitignored) |
| `out/week{N}/` | Per-week archive |

Paths resolve from repo root via `fftiers.root` in `main.R` (script location, `FFTIERS_ROOT` env, or `~/projects/fftiers` on Mac).

## 1. Season rollover (annual, ~August)

Precedent commits: `8b119b1` (2025), `6507e26` (2024) — now consolidated into `src/config.R`.

### Checklist

1. Edit **`src/config.R` only**:
   - `year` (numeric, e.g. `2027`)
   - `weekonetuesday` — **Tuesday before NFL Week 1** (in-season cron starts this day)
   - `season_end` — last in-season cron run (usually first Sunday in January; e.g. `2027-01-03` for 2026 season)
2. Grep: `grep -rn "OLD_YEAR" src/` — should return nothing except `config.R` and docstring examples.
3. Test: `cd src && Rscript main.R TRUE`

### Week 1 Tuesday

Search for the NFL season opener, then use the **Tuesday immediately before** that date for `weekonetuesday`. `thisweek` is 0 before that Tuesday (pre-draft).

## 2. Run pipeline

### Dependencies

```bash
# R packages (Workbench: apt may install r-cran-mclust)
Rscript -e 'install.packages(c("mclust","ggplot2"), repos="https://cloud.r-project.org")'
```

`src/api_key.py` must exist (one line: FantasyPros API key). **Never commit** — add to `.gitignore` if missing.

### Commands

```bash
cd src
Rscript main.R TRUE   # download fresh FantasyPros data + generate charts (~30s)
Rscript main.R FALSE  # regenerate charts from existing dat/ only (~15s)
```

`master.py` runs `main.R t` then `push-to-s3.py` (production cron). Use `python3 src/master.py --dry-run` to generate and preview S3 uploads without pushing.

### Outputs

- `out/current/png/weekly-*.png` → S3 `s3://fftiers/out/weekly-*.png`
- `out/current/csv/weekly-*.csv`, `out/current/txt/text_*.txt` — same basename mapping
- `push-to-s3.py` uploads **only** `out/current/png/`, `csv/`, `txt/` by **basename** to `s3://fftiers/out/<basename>` (matches website layout). Skips hidden files.

`main.R` clears `out/current/{png,csv,txt}/*` and removes root-level orphans in `out/current/`.

## 3. S3 deploy

### Workbench (cross-account)

Default Workbench role (`TitusContainerRole`) cannot write to personal `s3://fftiers`. Use a **`personal`** AWS profile:

`~/.aws/credentials`:
```ini
[personal]
aws_access_key_id = ...
aws_secret_access_key = ...
```

`~/.aws/config`:
```ini
[profile personal]
region = us-east-1
```

`push-to-s3.py` auto-selects `personal` when present; respects `AWS_PROFILE` if already set; otherwise falls back to default Workbench credentials.

### Safe access test (before full push)

```bash
echo "probe $(date -u +%Y-%m-%dT%H:%M:%SZ)" > /tmp/workbench-s3-probe.txt
AWS_PROFILE=personal aws s3 cp /tmp/workbench-s3-probe.txt s3://fftiers/out/_workbench-access-test.txt
AWS_PROFILE=personal aws s3api head-object --bucket fftiers --key out/_workbench-access-test.txt
AWS_PROFILE=personal aws s3 rm s3://fftiers/out/_workbench-access-test.txt
```

Do **not** run `push-to-s3.py` for access testing — it uploads all tier files.

### Preview push (no upload)

```bash
python3 src/push-to-s3.py --dry-run
```

### Full push

```bash
python3 src/push-to-s3.py
```

Confirm `out/current/{png,csv,txt}/` are populated before pushing.

### Bucket policy (personal AWS account)

Cross-account write from Workbench requires bucket policy on `fftiers` granting `arn:aws:iam::219382154434:role/TitusContainerRole` PutObject/GetObject/DeleteObject on `out/*`. Template: `src/fftiers-workbench-bucket-policy.json`. Merge with existing public-read policy.

Note: bucket policy alone may not suffice — Workbench role may lack outbound S3 permission to external buckets. **`personal` profile is the reliable path.**

## 4. Workbench cron (production)

Runs on this Workbench at **6:00 AM Pacific** (`TZ=America/Los_Angeles`). Schedule dates come from **`src/config.R`** (`weekonetuesday`, `season_end`) — no edits to `cron-run.sh` needed at rollover.

| Phase | When | Days |
|-------|------|------|
| Pre-draft | before `weekonetuesday` | Mon, Thu |
| In-season | `weekonetuesday` → `season_end` | Tue, Thu, Sun |
| Off-season | after `season_end` | none (wrapper exits) |

### Install

```bash
chmod +x src/cron-run.sh
mkdir -p logs
crontab src/crontab.example
crontab -l
```

Crontab fires daily at 6am; `src/cron-run.sh` gates on date + day-of-week. Uses `flock` to prevent overlap. Logs to `logs/cron.log`. Emails `boris.chen@gmail.com` on failure.

### Test

```bash
./src/cron-run.sh              # respects schedule (skips on wrong day)
./src/cron-run.sh --force      # run now regardless of schedule
python3 src/master.py --dry-run
```

### Workbench caveat

`/root` persists, but the container may sleep when idle — missed 6am runs are not caught up. Use EC2 if guaranteed uptime is needed.

### Post-season rollover

Update `weekonetuesday` and `season_end` in `src/config.R` next August. Cron picks up new dates automatically.

## 5. Production EC2 (optional fallback)

Cheapest reliable: **`t4g.micro`** (~$6/mo, 1 GB RAM) in `us-east-1`, Ubuntu, IAM role for S3 (no keys on instance). Copy `src/cron-run.sh` + `src/crontab.example` and adjust paths to `/home/ubuntu/projects/fftiers`.

Stop instance in offseason to save ~60% annual cost.

## Git hygiene

**Never commit:** `src/api_key.py`, `dat/`, `out/`, `logs/`, `~/.aws/credentials`

**Safe to commit:** `src/config.R`, `src/*.R`, `src/*.py`, `src/fftiers-workbench-bucket-policy.json`

Commit message style: `Update fftiers for YYYY with portable repo paths.` or `Use personal AWS profile for S3 uploads when available.`

## Troubleshooting

| Symptom | Fix |
|---------|-----|
| `Could not locate fftiers repo root` | Run via `Rscript main.R` from `src/`, or set `FFTIERS_ROOT` |
| `api_key.py` not found | Create `src/api_key.py` with FantasyPros key |
| PutObject AccessDenied (Workbench) | Use `personal` profile, not default Titus role |
| Wrong S3 filenames (`txttext_*`) | Re-run `main.R`; `push-to-s3.py` only uploads png/csv/txt subdirs |
| `thisweek` wrong | Check `weekonetuesday` matches Tuesday before Week 1 |
