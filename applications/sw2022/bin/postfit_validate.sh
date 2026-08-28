#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
project_root=$(CDPATH= cd -- "${script_dir}/../../.." && pwd)
app_root="${project_root}/applications/sw2022"
output_root="${app_root}/manifests/postfit_validation"
r_launcher="${project_root}/applications/bin/R45"
rscript_launcher="${project_root}/applications/bin/Rscript45"
library_root="${project_root}/.R-library/4.5"

fit_confirmed_stopped=false
if [ "$#" -gt 1 ]; then
  echo "Usage: applications/sw2022/bin/postfit_validate.sh [--fit-confirmed-stopped]" >&2
  exit 64
fi
if [ "$#" -eq 1 ]; then
  if [ "$1" != "--fit-confirmed-stopped" ]; then
    echo "Unknown argument: $1" >&2
    exit 64
  fi
  fit_confirmed_stopped=true
fi

active_fit=""
process_check=127
if command -v pgrep >/dev/null 2>&1; then
  set +e
  active_fit=$(pgrep -f 'applications/sw2022/R/03_fit_models[.]R' 2>/dev/null)
  process_check=$?
  set -e
fi
if [ "${process_check}" -eq 0 ] && [ -n "${active_fit}" ]; then
  echo "Refusing to reinstall sconjoint while 03_fit_models.R is active (PID ${active_fit})." >&2
  exit 75
fi
if [ "${process_check}" -ne 1 ] && [ "${fit_confirmed_stopped}" != true ]; then
  echo "The host process service could not verify that 03_fit_models.R has exited." >&2
  echo "After the active fit session has completed, rerun with --fit-confirmed-stopped." >&2
  exit 75
fi

mkdir -p "${output_root}"
started_utc=$(date -u '+%Y-%m-%dT%H:%M:%SZ')

finish_record() {
  exit_code=$?
  finished_utc=$(date -u '+%Y-%m-%dT%H:%M:%SZ')
  {
    echo "started_utc=${started_utc}"
    echo "finished_utc=${finished_utc}"
    echo "exit_status=${exit_code}"
    echo "process_check_code=${process_check}"
    echo "fit_confirmed_stopped=${fit_confirmed_stopped}"
  } > "${output_root}/driver_status.txt"
}
trap finish_record EXIT

echo "[1/4] Checking the tracked diff for whitespace errors."
git -C "${project_root}" diff --check > "${output_root}/git_diff_check.txt"
git -C "${project_root}" diff --cached --check >> "${output_root}/git_diff_check.txt"

echo "[2/4] Reinstalling the current package source into the project library."
if ! "${r_launcher}" CMD INSTALL \
  --preclean --no-multiarch --with-keep.source \
  --library="${library_root}" "${project_root}" \
  > "${output_root}/R_CMD_INSTALL.log" 2>&1; then
  tail -n 80 "${output_root}/R_CMD_INSTALL.log" >&2
  exit 1
fi
tail -n 12 "${output_root}/R_CMD_INSTALL.log"

echo "[3/4] Refreshing the pinned R/Torch environment record."
"${rscript_launcher}" "${app_root}/R/00_verify_environment.R" \
  > "${output_root}/environment_verification.log" 2>&1
tail -n 8 "${output_root}/environment_verification.log"

echo "[4/4] Hashing artifacts, parsing sources, and running focused tests."
if ! "${rscript_launcher}" "${app_root}/R/08_postfit_reproducibility.R" \
  > "${output_root}/focused_validation.log" 2>&1; then
  tail -n 120 "${output_root}/focused_validation.log" >&2
  exit 1
fi
tail -n 40 "${output_root}/focused_validation.log"

echo "Post-fit validation record: ${output_root}"
