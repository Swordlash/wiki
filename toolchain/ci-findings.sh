#!/bin/sh
# Run one hspec suite as the nightly search for races runs it, and write
# what failed into the job's summary (.github/workflows/nightly.yml; WP5.114
# and WP5.122 of docs/PLAN.md). A failure there is a finding, not a flake:
# the summary keeps what is needed to find it again.
#
#   sh toolchain/ci-findings.sh <title> <command> [args...]
#
# The command's output passes through as it comes and is kept in a log.
# The title names the run in the summary ($GITHUB_STEP_SUMMARY, when there
# is one), and should carry the seed the command was given. A suite that
# passed gets one line: its title and how many examples ran. A suite that
# failed gets its title, the exit status, the counts, each failed example
# with hspec's line to run it again (its --match and --seed), and then
# hspec's report of each failure (for a property, the shrunk run and what
# broke) folded under a <details>, cut at 400 lines. A suite that ran in
# several shards reports once a shard; the counts are added up, and every
# shard's failures are listed.
#
# A suite that ran no example at all fails here too: a --match that
# matches nothing, or a runner that stopped before the suite reported,
# would otherwise pass in silence. The last 60 lines of its output then go
# to the summary, as they do for a command that failed with no failed
# example (the runner's deadline, errors in the page, a crashed shard).
#
# It exits with the command's status when that is not 0, 1 when the
# command passed but ran no example, and 0 otherwise.
set -eu

title=${1:?usage: ci-findings.sh <title> <command> [args...]}
shift
[ "$#" -gt 0 ] || { echo "ci-findings.sh: no command to run for $title" >&2; exit 2; }

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT HUP INT TERM

# The status goes through a file: a pipeline's status is its last
# command's (tee's), and dash has no pipefail.
{
  status=0
  "$@" || status=$?
  echo "$status" > "$work/status"
} 2>&1 | tee "$work/raw"
status=$(cat "$work/status" 2>/dev/null || echo 1)
esc=$(printf '\033')
sed "s/$esc\[[0-9;]*m//g" "$work/raw" > "$work/log"

# hspec's last line of each report: "81 examples, 0 failures", with
# ", 2 pending" at times, once a shard.
counts=$(
  awk '
    /^[0-9]+ examples?, [0-9]+ failures?/ { examples += $1; failures += $3; reports++ }
    END { printf "%d %d %d\n", examples, failures, reports }
  ' "$work/log"
)
examples=${counts%% *}
rest=${counts#* }
failures=${rest%% *}
reports=${rest#* }

if [ "$status" -eq 0 ] && [ "$examples" -gt 0 ]; then
  verdict=passed
else
  verdict=failed
  [ "$status" -ne 0 ] || status=1
fi

summary() {
  [ -z "${GITHUB_STEP_SUMMARY:-}" ] || cat >> "$GITHUB_STEP_SUMMARY"
}

if [ "$verdict" = passed ]; then
  printf -- '- %s: passed, %s examples\n' "$title" "$examples" | summary
  exit 0
fi

# One write of the whole block, so that runs side by side do not mix.
{
  printf '\n#### %s: failed\n\n' "$title"
  printf 'Exit status %s; %s examples, %s failures, in %s reports.\n\n' "$status" "$examples" "$failures" "$reports"
  if [ "$examples" -eq 0 ] || ! grep -q '^  To rerun use: ' "$work/log"; then
    # No example ran, or the command failed with no failed example: the
    # runner's deadline, errors in the page, a shard that crashed.
    if [ "$examples" -eq 0 ]; then
      printf 'No example ran. The output ends:\n\n~~~~\n'
    else
      printf 'No example failed, but the command did. The output ends:\n\n~~~~\n'
    fi
    tail -n 60 "$work/log"
    printf '~~~~\n'
  else
    # Each failed example's name (hspec's "  1) path") and the line to run
    # it again, in the order hspec gives them.
    awk '
      /^  [0-9]+\) / { sub(/^  [0-9]+\) /, ""); name = $0; next }
      /^  To rerun use: / { sub(/^  To rerun use: /, ""); printf "- %s\n  `%s`\n", name, $0 }
    ' "$work/log"
    printf '\n<details><summary>The failures as hspec reported them</summary>\n\n~~~~\n'
    awk '
      /^Failures:$/ { on = 1 }
      /^Randomized with seed / { if (on) print; on = 0; next }
      on { print }
    ' "$work/log" | head -n 400
    printf '~~~~\n\n</details>\n'
  fi
} | summary
exit "$status"
