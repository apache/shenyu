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

WORKFLOW_FILE="${1:-.github/workflows/docker-publish.yml}"
readonly WORKFLOW_FILE

ruby - "${WORKFLOW_FILE}" <<'RUBY'
require "yaml"

workflow = YAML.safe_load(File.read(ARGV.fetch(0)), aliases: true)
events = workflow["on"] || workflow[true]
push = events.fetch("push")
patterns = push.fetch("paths-ignore")
flags = File::FNM_PATHNAME | File::FNM_DOTMATCH | File::FNM_EXTGLOB

def ignored?(patterns, path, flags)
  patterns.any? do |pattern|
    if pattern.end_with?("/**")
      path.start_with?(pattern.delete_suffix("**"))
    else
      File.fnmatch?(pattern, path, flags)
    end
  end
end

ignored_paths = [
  ".github/workflows/ci.yml",
  "actions/setup-java-with-retry/action.yml",
  "shenyu-e2e/shenyu-e2e-case/pom.xml",
  "shenyu-integrated-test/shenyu-integrated-test-grpc/pom.xml",
  "shenyu-examples/shenyu-examples-http/pom.xml"
]
runtime_paths = [
  "pom.xml",
  "db/init/mysql/schema.sql",
  "shenyu-admin/src/main/java/Admin.java",
  "shenyu-plugin/shenyu-plugin-api/src/main/java/ShenyuPlugin.java",
  "shenyu-dist/shenyu-admin-dist/docker/Dockerfile"
]

ignored_paths.each do |path|
  raise "Expected #{path} to skip GHCR publishing" unless ignored?(patterns, path, flags)
end

runtime_paths.each do |path|
  raise "Expected #{path} to trigger GHCR publishing" if ignored?(patterns, path, flags)
end

raise "Release tag publishing must remain enabled" unless push.fetch("tags").include?("v*.*.*")

puts "GHCR publish path tests passed"
RUBY
