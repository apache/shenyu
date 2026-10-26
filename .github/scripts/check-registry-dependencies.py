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

"""Assert registry boundaries against Maven's resolved runtime dependency trees."""

import sys
from pathlib import Path

PROVIDERS = ("zookeeper", "etcd", "nacos", "apollo", "eureka", "kubernetes")
COMPATIBILITY_PROVIDERS = set(PROVIDERS) - {"apollo"}
LEAN_MODULES = (
    "shenyu-registry/shenyu-registry-api",
    "shenyu-registry/shenyu-registry-core",
    "shenyu-sdk/shenyu-sdk-core",
    "shenyu-sdk/shenyu-sdk-feign",
    "shenyu-spring-boot-starter/shenyu-spring-boot-starter-sdk",
    "shenyu-spring-boot-starter/shenyu-spring-boot-starter-sdk-feign",
)
IMPLEMENTATION_MODULES = tuple("shenyu-registry/shenyu-registry-" + name for name in PROVIDERS)
COMPATIBILITY_MODULE = "shenyu-spring-boot-starter/shenyu-spring-boot-starter-registry"
MODULES = LEAN_MODULES + IMPLEMENTATION_MODULES + (COMPATIBILITY_MODULE,)
CLIENT_GROUPS = (
    "org.apache.zookeeper", "org.apache.curator", "io.etcd", "com.alibaba.nacos",
    "com.ecwid.consul", "com.orbitz.consul", "com.tencent.polaris",
    "com.netflix.eureka", "io.kubernetes",
)
TREE_FILE = "target/registry-dependencies.tgf"


def read_tree(path):
    """Read node coordinates, excluding the project node and the edge section."""
    nodes = path.read_text(encoding="utf-8").split("#", 1)[0].splitlines()
    if not nodes:
        raise ValueError("empty dependency tree: " + str(path))
    artifacts = set()
    for node in nodes[1:]:
        fields = node.split(" ", 1)
        if len(fields) != 2 or len(fields[1].split(":")) < 4:
            raise ValueError("invalid dependency coordinate: " + node)
        artifacts.add(tuple(fields[1].split(":")[:2]))
    return artifacts


def check_module(module, artifacts):
    errors = []
    implementations = {artifact.removeprefix("shenyu-registry-")
                       for group, artifact in artifacts
                       if group == "org.apache.shenyu" and artifact.startswith("shenyu-registry-")
                       and artifact not in {"shenyu-registry-api", "shenyu-registry-core"}}
    if module in LEAN_MODULES:
        if implementations:
            errors.append("registry implementations leaked into " + module + ": " + str(sorted(implementations)))
        clients = {group + ":" + artifact for group, artifact in artifacts
                   if any(group == prefix or group.startswith(prefix + ".") for prefix in CLIENT_GROUPS)}
        if clients:
            errors.append("registry clients leaked into " + module + ": " + str(sorted(clients)))
    if module in IMPLEMENTATION_MODULES and ("org.apache.shenyu", "shenyu-registry-core") in artifacts:
        errors.append(module + " depends on registry-core instead of registry-api")
    if module == COMPATIBILITY_MODULE:
        missing = COMPATIBILITY_PROVIDERS - implementations
        if missing:
            errors.append("compatibility starter lost providers: " + str(sorted(missing)))
    return errors


def main():
    if sys.argv[1:] == ["--modules"]:
        print(",".join(MODULES))
        return 0
    root = Path(__file__).resolve().parents[2]
    errors = []
    for module in MODULES:
        path = root / module / TREE_FILE
        try:
            artifacts = read_tree(path)
            errors.extend(check_module(module, artifacts))
        except (OSError, ValueError) as error:
            errors.append(str(error))
    if errors:
        print("\n".join(errors), file=sys.stderr)
        return 1
    print("Registry dependency boundaries passed for " + str(len(MODULES)) + " runtime trees.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
