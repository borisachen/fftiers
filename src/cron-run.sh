#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
export FFTIERS_ROOT="$REPO_ROOT"
export TZ=America/Los_Angeles
export PATH="/apps/default-python/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

MAIL_TO="boris.chen@gmail.com"
LOG_FILE="${REPO_ROOT}/logs/cron.log"
LAST_RUN_MARKER="${REPO_ROOT}/logs/.last_run_date"
TARGET_HOUR="06"
FORCE=false

for arg in "$@"; do
	case "$arg" in
		--force) FORCE=true ;;
	esac
done

config_val() {
	Rscript -e "source('${REPO_ROOT}/src/config.R'); cat(${1})" 2>/dev/null
}

SEASON_START=$(config_val weekonetuesday)
SEASON_END=$(config_val season_end)

send_mail() {
	local subject="$1"
	local body="$2"
	{
		printf 'To: %s\n' "$MAIL_TO"
		printf 'Subject: %s\n' "$subject"
		printf 'Content-Type: text/plain; charset=utf-8\n'
		printf '\n'
		printf '%s\n' "$body"
	} | /usr/sbin/sendmail "$MAIL_TO" || true
}

on_exit() {
	local rc=$?
	if [[ "$rc" -ne 0 ]]; then
		local log_tail=""
		if [[ -f "$LOG_FILE" ]]; then
			log_tail=$(tail -n 40 "$LOG_FILE")
		fi
		send_mail "[fftiers] cron FAILED (rc=$rc)" \
			"fftiers cron-run.sh failed at $(TZ=America/Los_Angeles date '+%Y-%m-%d %H:%M:%S %Z')

log tail:
${log_tail}"
	fi
}
trap on_exit EXIT

should_run_today() {
	local today dow
	today=$(date +%F)
	dow=$(date +%u)

	if [[ "$today" < "$SEASON_START" ]]; then
		[[ "$dow" == "1" || "$dow" == "4" ]] && return 0
		return 1
	fi
	if [[ "$today" > "$SEASON_END" ]]; then
		return 1
	fi
	[[ "$dow" == "2" || "$dow" == "4" || "$dow" == "7" ]] && return 0
	return 1
}

phase_label() {
	local today
	today=$(date +%F)
	if [[ "$today" < "$SEASON_START" ]]; then
		echo "pre-draft"
	elif [[ "$today" > "$SEASON_END" ]]; then
		echo "off-season"
	else
		echo "in-season"
	fi
}

# The cron daemon on this box schedules in system-local time (UTC), and
# ignores the TZ= line in the crontab for scheduling purposes (it only
# sets the job's environment). To fire reliably at 6:00 AM Pacific across
# DST changes, the crontab triggers this script every 5 minutes, and the
# gating below decides -- using Pacific wall-clock time -- whether this
# is actually the 6am slot, running at most once per calendar day.
if [[ "$FORCE" != true ]]; then
	if ! should_run_today; then
		echo "$(date '+%Y-%m-%d %H:%M:%S %Z') skip ($(phase_label), dow=$(date +%u))"
		exit 0
	fi

	current_hour=$(date +%H)
	if [[ "$current_hour" != "$TARGET_HOUR" ]]; then
		exit 0
	fi

	today=$(date +%F)
	if [[ -f "$LAST_RUN_MARKER" && "$(cat "$LAST_RUN_MARKER")" == "$today" ]]; then
		exit 0
	fi
fi

echo "$(date '+%Y-%m-%d %H:%M:%S %Z') start ($(phase_label), force=$FORCE)"
date +%F > "$LAST_RUN_MARKER"
exec python3 "${REPO_ROOT}/src/master.py"
