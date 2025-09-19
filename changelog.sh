#!/bin/sh
# ----------------------------------------------------------------------------
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

# Configuration
FROM_TAG=""
TO_TAG="HEAD"
OUTPUT_FILE="CHANGELOG.md"
IGNORE_TYPES=("sync frontend" "merge")  # Use arrays to manage ignored types more clearly
DATE_FORMAT="%Y-%m-%d"

# Function: Handle error and exit
handle_error() {
    echo "Error: $1" >&2
    exit 1
}

# Check if git is installed
command -v git >/dev/null 2>&1 || handle_error "git is not installed."

# Generate type title
generate_type_header() {
    local type="$1"
    case "$type" in
        feat) echo "### ✨ New Features";;
        fix) echo "###  Bug Fixes";;  # Modify the title to the more common "Bug Fixes"
        improve) echo "### ⚡ Improvements";;
        chore) echo "###  Chore";;
        refactor) echo "### ♻️ Refactor";;
        docs) echo "###  Documentation";;
        style) echo "###  Styles";;
        perf) echo "### ⚡ Performance";;
        test) echo "### ✅ Tests";;
        build) echo "###  Build";;
        ci) echo "###  CI";;
        issue) echo "###  Issues";;
        task) echo "###  Tasks";;
        other) echo "###  Other Changes";;
        *) echo "### ❓ Unknown Type: $type";;
    esac
}

# Initialize the change log
initialize_changelog() {
    local from="$1"
    local to="$2"
    local date=$(date +"$DATE_FORMAT")

    echo "# Changelog" > "$OUTPUT_FILE"
    echo "" >> "$OUTPUT_FILE"
    echo "## [$to] - $date" >> "$OUTPUT_FILE"
    echo "" >> "$OUTPUT_FILE"
}

# Normalized types
normalize_type() {
    local type="$1"
    type=$(echo "$type" | tr '[:upper:]' '[:lower:]')
    case "$type" in
        feature|feat) echo "feat";;
        bugfix|fix) echo "fix";;  # Map bugfix to fix
        improve|improvement) echo "improve";;
        *) echo "$type";;
    esac
}

# Should types be ignored
should_ignore_type() {
    local message="$1"

    # Exact match ignore submission information
    for ignore_message in "${IGNORE_TYPES[@]}"; do
        if [[ "$message" == "$ignore_message" ]]; then
            return 0
        fi
    done

    # Ignore commits starting with "Bump" (case insensitive)
    if echo "$message" | grep -qi "^bump"; then
        return 0
    fi
    return 1
}

# Extract type
extract_type() {
    local message="$1"
    local lower_message=$(echo "$message" | tr '[:upper:]' '[:lower:]')
    local type=$(echo "$lower_message" | grep -oE '^\[type:([a-zA-Z]+)\]|^type:([a-zA-Z]+)|^\[([a-zA-Z]+)\]|^(feat|fix|docs|style|refactor|perf|test|build|ci|chore|revert|feature|bugfix|improve|task|issue):?' | sed -E 's/^\[type://; s/\]//g; s/^type://; s/^\[//; s/\]//; s/://')
    if [ -z "$type" ]; then
        echo "other"
    else
        normalize_type "$type"
    fi
}

# Clean up submission information
clean_commit_message() {
    local message="$1"
    echo "$message" | sed -E 's/^\[type:[a-zA-Z]+\]//; s/^type:[a-zA-Z]+//; s/^\[[a-zA-Z]+\]//; s/^[a-zA-Z]+://; s/^ *//'
}

# Generate a change log
generate_changelog() {
    local from="$1"
    local to="$2"
    local tmp_file

    initialize_changelog "$from" "$to"

    git log "$from..$to" --pretty=format:'%s|%h|%an' | while IFS='|' read -r message hash author; do
        type=$(extract_type "$message")

        if should_ignore_type "$message"; then # Pass the complete message to should_ignore_type
            continue
        fi

        clean_message=$(clean_commit_message "$message")

        tmp_file=$(mktemp) || handle_error "Failed to create temporary file."
        echo "- ${clean_message} (${hash}) by ${author}" > "$tmp_file"

        if [ ! -f "tmp_${type}.txt" ]; then
            mv "$tmp_file" "tmp_${type}.txt"
        else
            cat "$tmp_file" >> "tmp_${type}.txt"
            rm "$tmp_file"
        fi
    done

    # Merge all types of changes
    for type in feat fix improve chore refactor docs style perf test build ci issue task other; do
        if [ -f "tmp_${type}.txt" ]; then
            generate_type_header "$type" >> "$OUTPUT_FILE"
            echo "" >> "$OUTPUT_FILE"
            cat "tmp_${type}.txt" >> "$OUTPUT_FILE"
            echo "" >> "$OUTPUT_FILE"
            rm "tmp_${type}.txt"
        fi
    done
}

# Generate from the recent tag
generate_from_latest_tag() {
    local latest_tag
    latest_tag=$(git describe --tags --abbrev=0 2>/dev/null)

    if [ -z "$latest_tag" ]; then
        echo "No tags found. Generating changelog for all commits."
        generate_changelog "" "$TO_TAG"
    else
        generate_changelog "$latest_tag" "$TO_TAG"
    fi
}

# How to use
usage() {
    echo "Usage: $0 [-f from_tag] [-t to_tag] [-o output_file]"
    echo "  -f: Starting tag (default: latest tag)"
    echo "  -t: Ending tag (default: HEAD)"
    echo "  -o: Output file (default: $OUTPUT_FILE)"
    exit 1
}

# Resolve command line parameters
while getopts "f:t:o:h" opt; do
    case "$opt" in
        f) FROM_TAG="$OPTARG";;
        t) TO_TAG="$OPTARG";;
        o) OUTPUT_FILE="$OPTARG";;
        h) usage;;
        \?) usage;;
    esac
done

# Verify that the tag exists
if [ ! -z "$FROM_TAG" ] && ! git rev-parse --verify "$FROM_TAG" > /dev/null 2>&1 ; then
    handle_error "Invalid from tag: $FROM_TAG"
fi

if [ ! -z "$TO_TAG" ] && ! git rev-parse --verify "$TO_TAG" > /dev/null 2>&1 ; then
    handle_error "Invalid to tag: $TO_TAG"
fi

# Execute generation
if [ -z "$FROM_TAG" ]; then
    generate_from_latest_tag
else
    generate_changelog "$FROM_TAG" "$TO_TAG"
fi

echo "Changelog has been generated in $OUTPUT_FILE"