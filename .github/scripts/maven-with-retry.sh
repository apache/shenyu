#!/usr/bin/env bash
#
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

set -euo pipefail

log_file="$(mktemp)"
trap 'rm -f "${log_file}"' EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

is_transfer_failure() {
  awk '
    function finish_error(    count, transfers, i) {
      if (error == "") {
        return
      }
      errors++
      count = split(error, transfers, /Could not transfer artifact /)
      if (count < 2 || error ~ /Could not find artifact /) {
        invalid = 1
      }
      for (i = 2; i <= count; i++) {
        if (transfers[i] !~ /from\/to central \(https:\/\/repo\.maven\.apache\.org\/maven2\)/ ||
            transfers[i] !~ /Connection reset|([Ss]tatus code:|[Ss]tatus:|Return code is:)[ ]*(429|502)([^0-9]|$)/) {
          invalid = 1
        }
      }
      error = ""
    }
    {
      # Maven and mvnd both color and prefix their error output.
      gsub(/\033\[[0-9;]*[mK]/, "")
      sub(/\r$/, "")
      if (!sub(/^.*\[ERROR\][ ]*/, "")) {
        next
      }
      if ($0 ~ /^\[Help [0-9]+\] https?:\/\//) {
        finish_error()
        references++
        if ($0 !~ /\/MAVEN\/(DependencyResolutionException|PluginResolutionException|ProjectBuildingException|UnresolvableModelException)$/) {
          invalid = 1
        }
        next
      }
      # Account for every POM error, including errors without a Help reference.
      if ($0 ~ /^The project .* has [0-9]+ errors?$/) {
        finish_error()
        sub(/^.* has /, "")
        model_errors += $1
        next
      }
      if ($0 ~ /^(Failed to execute goal |Plugin .* could not be resolved|Non-resolvable (parent|import) POM)/) {
        finish_error()
        if ($0 ~ /^Non-resolvable /) {
          classified_model_errors++
        }
        error = $0
      } else if (error != "") {
        error = error " " $0
      }
      if ($0 ~ /-> \[Help [0-9]+\]/) {
        finish_error()
      }
    }
    END {
      finish_error()
      exit !(errors > 0 && references > 0 && !invalid && model_errors == classified_model_errors)
    }
  ' "${log_file}"
}

for attempt in 1 2 3; do
  # Capture both statuses before another command replaces PIPESTATUS.
  set +e
  "$@" 2>&1 | tee "${log_file}"
  statuses=("${PIPESTATUS[@]}")
  set -e

  if [[ "${statuses[0]}" -eq 0 ]]; then
    exit "${statuses[1]}"
  fi
  if [[ "${statuses[0]}" -ne 1 || "${statuses[1]}" -ne 0 || "${attempt}" -eq 3 ]] || ! is_transfer_failure; then
    exit "${statuses[0]}"
  fi

  echo "Maven Central transfer failed; retrying in $((attempt * 10)) seconds (${attempt}/3)." >&2
  sleep "$((attempt * 10))"
done
