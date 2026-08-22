# borischen.co Fantasy Football Tiers

Code for generating the tiers behind [www.borischen.co](https://www.borischen.co). All player data comes from [FantasyPros](https://www.fantasypros.com).

## Quick start

```bash
# One-time: create src/api_key.py with your FantasyPros API key (never commit)
# One-time: R packages
Rscript -e 'install.packages(c("mclust","ggplot2"), repos="https://cloud.r-project.org")'

cd src
Rscript main.R TRUE    # download + generate (~30s)
python3 push-to-s3.py  # upload to s3://fftiers/out/
```

Or run the full pipeline: `python3 src/master.py`

## Season rollover (~August each year)

**Edit [`src/config.R`](src/config.R) only.** Everything else reads from it.

| Variable | What to set |
|----------|-------------|
| `year` | NFL season year (e.g. `2027`) |
| `weekonetuesday` | Tuesday immediately before Week 1 kickoff (in-season cron starts this day) |
| `season_end` | Last day of in-season cron (usually first Sunday in January after playoffs) |

### Example: 2027 season

```r
year <- 2027
weekonetuesday <- "2027-09-07"  # Tuesday before Week 1
season_end <- "2028-01-02"      # Last Tue/Thu/Sun cron run
```

### Checklist

1. Update the three variables in `src/config.R`
2. Verify: `grep -rn "2026" src/` — only `config.R` (and docs) should match
3. Test generate: `cd src && Rscript main.R TRUE`
4. Test push: `python3 src/push-to-s3.py --dry-run`
5. Cron picks up new dates automatically — no changes to `cron-run.sh` needed

### Cron schedule (automatic from config.R)

| Phase | When | Days (6:00 AM Pacific) |
|-------|------|------------------------|
| Pre-draft | before `weekonetuesday` | Monday, Thursday |
| In-season | `weekonetuesday` through `season_end` | Tuesday, Thursday, Sunday |
| Off-season | after `season_end` | none (wrapper exits) |

## Cron setup (Workbench)

```bash
chmod +x src/cron-run.sh
mkdir -p logs
crontab src/crontab.example   # edit path in file if repo is not at /root/dev/fftiers
crontab -l
```

Logs: `logs/cron.log`. Failures email `boris.chen@gmail.com`.

```bash
./src/cron-run.sh              # respects schedule (skips on wrong day)
./src/cron-run.sh --force      # run now regardless of schedule
```

**Workbench caveat:** the container may sleep when idle — missed 6am runs are not caught up.

## Repo layout

| Path | Purpose |
|------|---------|
| `src/config.R` | **Yearly config** — `year`, `weekonetuesday`, `season_end` |
| `src/main.R` | Download + chart generation |
| `src/master.py` | Cron entry: generate then S3 push |
| `src/cron-run.sh` | Date-aware cron wrapper (reads `config.R`) |
| `src/crontab.example` | Crontab template |
| `src/push-to-s3.py` | Upload `out/current/{png,csv,txt}/` → `s3://fftiers/out/` |
| `src/api_key.py` | FantasyPros API key (**gitignored, never commit**) |

## AWS / S3

From Workbench, use a `personal` AWS profile in `~/.aws/credentials` — the default Workbench role cannot write to `s3://fftiers`. See [`.cursor/skills/fftiers-ops/SKILL.md`](.cursor/skills/fftiers-ops/SKILL.md) for full ops docs.
