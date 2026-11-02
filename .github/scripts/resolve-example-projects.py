#!/usr/bin/env python3
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

"""Select every example image consumed by an integration case, before building."""

import re
import sys
import xml.etree.ElementTree as ET
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
NS = {"m": "http://maven.apache.org/POM/4.0.0"}
IMAGE = re.compile(r'''(?:image:\s*|kind load docker-image\s+)["']?(?:apache/)?(shenyu-examples?-[\w-]+):''')


def resolve(case, root=ROOT):
    """Fail closed when a new deployment has no identifiable example image."""
    if not re.fullmatch(r"shenyu-integrated-test-[a-z0-9-]+", case):
        raise ValueError(f"Invalid integration case: {case}")
    directory = root / "shenyu-integrated-test" / case
    if not (directory / "pom.xml").is_file():
        raise ValueError(f"Integration case does not exist: {case}")
    images = set()
    for path in directory.rglob("*"):
        if "target" not in path.parts and path.suffix in (".sh", ".yaml", ".yml"):
            images.update(IMAGE.findall(path.read_text()))
    if not images:
        raise ValueError(f"No example images found for {case}; inspect its deployment before enabling CI")
    artifacts = {
        ET.parse(path).getroot().findtext("m:artifactId", namespaces=NS)
        for path in (root / "shenyu-examples").rglob("pom.xml")
        if "target" not in path.parts
    }
    if images - artifacts:
        raise ValueError(f"Example images without Maven modules: {sorted(images - artifacts)}")
    return ",".join(":" + image for image in sorted(images))


if __name__ == "__main__":
    print(resolve(sys.argv[1]))
