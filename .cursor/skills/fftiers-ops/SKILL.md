---
name: fftiers-ops
description: Operate the borischen.co fftiers pipeline — season rollover, run main.R, S3 deploy from Workbench or EC2. Use when updating the NFL year, running tiers, pushing to s3://fftiers, FantasyPros downloads, or Workbench/AWS setup for this repo.
---

# fftiers Ops

Fantasy football tier charts for [borischen.co](https://www.borischen.co). Data from FantasyPros API; outputs go to `out/current/` then `s3://fftiers/out/`.

## Repo layout

| Path | Purpose |
|------|---------|
| `src/main.R` | Entry point: download + chart generation |
| `src/ff-functions.R` | Download helpers, clustering, ggplot output |
| `src/fp_api.py` | FantasyPros API → JSON/CSV (`src/api_key.py` required, **never commit**) |
| `src/push-to-s3.py` | Upload `out/current/` → `s3://fftiers/out/` |
| `src/master.py` | Cron wrapper: `main.R` then `push-to-s3.py` |
| `dat/{year}/` | Raw downloaded data (gitignored) |
| `out/current/{png,csv,txt}/` | Website-facing outputs (gitignored) |
| `out/week{N}/` | Per-week archive |

Paths resolve from repo root via `fftiers.root` in `main.R` (script location, `FFTIERS_ROOT` env, or `~/projects/fftiers` on Mac).

## 1. Season rollover (annual, ~August)

Precedent commits: `8b119b1` (2025), `6507e26` (2024) — small diff, same pattern each year.

### Checklist

1. Set `year` in `src/main.R` (numeric, e.g. `2026`).
2. Set `weekonetuesday` to the **Tuesday before NFL Week 1** (opening kickoff is usually Thursday; 2026 opened Wednesday so Tuesday was `2026-09-08`).
3. Update draft chart titles in `src/ff-functions.R` (`error.bar.plot`: `"YYYY Draft - ..."` strings).
4. Data paths use `fftiers.root` + `year` — no hardcoded `dat/2025` left behind.
5. `src/fp_api.py` `make_path(year)` creates `dat/{year}/` relative to repo root.
6. Grep: `grep -rn "OLD_YEAR" src/` — should return nothing except docstring examples.
7. Test: `cd src && Rscript main.R TRUE`

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

`master.py` runs `main.R t` then `push-to-s3.py` (production cron).

### Outputs

- `out/current/png/weekly-*.png` → S3 `s3://fftiers/out/weekly-*.png`
- `out/current/csv/weekly-*.csv`, `out/current/txt/text_*.txt` — same basename mapping
- `push-to-s3.py` uploads by **filename only** (not subdir path), which matches the website layout

### Stale file cleanup

If `out/current/` has root-level junk (`pngweekly-*`, `csvweekly-*`, `txttext_*`) from old path bugs:

```bash
rm -f out/current/pngweekly-* out/current/csvweekly-* out/current/txttext_*
```

`main.R` clears `out/current/{png,csv,txt}/*` but not root-level orphans. Correct files live only in subdirs.

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

### Full push

```bash
python3 src/push-to-s3.py
```

Confirm `out/current/` has only files under `png/`, `csv/`, `txt/` (no root junk) before pushing.

### Bucket policy (personal AWS account)

Cross-account write from Workbench requires bucket policy on `fftiers` granting `arn:aws:iam::219382154434:role/TitusContainerRole` PutObject/GetObject/DeleteObject on `out/*`. Template: `src/fftiers-workbench-bucket-policy.json`. Merge with existing public-read policy.

Note: bucket policy alone may not suffice — Workbench role may lack outbound S3 permission to external buckets. **`personal` profile is the reliable path.**

## 4. Production EC2 (optional)

Cheapest reliable: **`t4g.micro`** (~$6/mo, 1 GB RAM) in `us-east-1`, Ubuntu, IAM role for S3 (no keys on instance).

Cron (`src/mycron.txt` pattern):
```
0 * * * * python /home/ubuntu/projects/fftiers/src/master.py
```

Stop instance in offseason to save ~60% annual cost.

## Git hygiene

**Never commit:** `src/api_key.py`, `dat/`, `out/`, `~/.aws/credentials`

**Safe to commit:** `src/*.R`, `src/*.py`, `src/fftiers-workbench-bucket-policy.json`

Commit message style: `Update fftiers for YYYY with portable repo paths.` or `Use personal AWS profile for S3 uploads when available.`

## Troubleshooting

| Symptom | Fix |
|---------|-----|
| `Could not locate fftiers repo root` | Run via `Rscript main.R` from `src/`, or set `FFTIERS_ROOT` |
| `api_key.py` not found | Create `src/api_key.py` with FantasyPros key |
| PutObject AccessDenied (Workbench) | Use `personal` profile, not default Titus role |
| Wrong S3 filenames (`txttext_*`) | Remove root junk; re-run `main.R`; verify `file.path()` in `ff-functions.R` |
| `thisweek` wrong | Check `weekonetuesday` matches Tuesday before Week 1 |
