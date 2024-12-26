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

# 配置
FROM_TAG=""
TO_TAG="HEAD"
OUTPUT_FILE="CHANGELOG.md"
IGNORE_TYPES=("sync frontend" "merge")  # 使用数组更清晰地管理忽略类型
DATE_FORMAT="%Y-%m-%d"

# 函数：处理错误并退出
handle_error() {
    echo "Error: $1" >&2
    exit 1
}

# 检查 git 是否安装
command -v git >/dev/null 2>&1 || handle_error "git is not installed."

# 生成类型标题
generate_type_header() {
    local type="$1"
    case "$type" in
        feat) echo "### ✨ New Features";;
        fix) echo "###  Bug Fixes";;  # 修改标题为更常见的 "Bug Fixes"
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

# 初始化变更日志
initialize_changelog() {
    local from="$1"
    local to="$2"
    local date=$(date +"$DATE_FORMAT")

    echo "# Changelog" > "$OUTPUT_FILE"
    echo "" >> "$OUTPUT_FILE"
    echo "## [$to] - $date" >> "$OUTPUT_FILE"
    echo "" >> "$OUTPUT_FILE"
}

# 规范化类型
normalize_type() {
    local type="$1"
    type=$(echo "$type" | tr '[:upper:]' '[:lower:]')
    case "$type" in
        feature|feat) echo "feat";;
        bugfix|fix) echo "fix";;  # 将 bugfix 映射到 fix
        improve|improvement) echo "improve";;
        *) echo "$type";;
    esac
}

# 是否应该忽略类型
should_ignore_type() {
    local message="$1"

    # 精确匹配忽略的提交信息
    for ignore_message in "${IGNORE_TYPES[@]}"; do
        if [[ "$message" == "$ignore_message" ]]; then
            return 0
        fi
    done

    # 忽略以 "Bump" 开头的提交（不区分大小写）
    if echo "$message" | grep -qi "^bump"; then
        return 0
    fi
    return 1
}

# 提取类型
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

# 清理提交信息
clean_commit_message() {
    local message="$1"
    echo "$message" | sed -E 's/^\[type:[a-zA-Z]+\]//; s/^type:[a-zA-Z]+//; s/^\[[a-zA-Z]+\]//; s/^[a-zA-Z]+://; s/^ *//'
}

# 生成变更日志
generate_changelog() {
    local from="$1"
    local to="$2"
    local tmp_file

    initialize_changelog "$from" "$to"

    git log "$from..$to" --pretty=format:'%s|%h|%an' | while IFS='|' read -r message hash author; do
        type=$(extract_type "$message")

        if should_ignore_type "$message"; then # 传递完整的message给should_ignore_type
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

    # 合并所有类型的变更
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

# 从最近的标签生成
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

# 使用方法
usage() {
    echo "Usage: $0 [-f from_tag] [-t to_tag] [-o output_file]"
    echo "  -f: Starting tag (default: latest tag)"
    echo "  -t: Ending tag (default: HEAD)"
    echo "  -o: Output file (default: $OUTPUT_FILE)"
    exit 1
}

# 解析命令行参数
while getopts "f:t:o:h" opt; do
    case "$opt" in
        f) FROM_TAG="$OPTARG";;
        t) TO_TAG="$OPTARG";;
        o) OUTPUT_FILE="$OPTARG";;
        h) usage;;
        \?) usage;;
    esac
done

# 验证标签是否存在
if [ ! -z "$FROM_TAG" ] && ! git rev-parse --verify "$FROM_TAG" > /dev/null 2>&1 ; then
    handle_error "Invalid from tag: $FROM_TAG"
fi

if [ ! -z "$TO_TAG" ] && ! git rev-parse --verify "$TO_TAG" > /dev/null 2>&1 ; then
    handle_error "Invalid to tag: $TO_TAG"
fi

# 执行生成
if [ -z "$FROM_TAG" ]; then
    generate_from_latest_tag
else
    generate_changelog "$FROM_TAG" "$TO_TAG"
fi

echo "Changelog has been generated in $OUTPUT_FILE"