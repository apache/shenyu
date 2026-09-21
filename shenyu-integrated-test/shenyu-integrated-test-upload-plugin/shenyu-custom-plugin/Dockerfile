# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

FROM eclipse-temurin:17-centos7
ENV APP_NAME shenyu-integrated-test-custom-plugin
LABEL org.opencontainers.image.title="${APP_NAME}" \
      org.opencontainers.image.source="https://github.com/apache/shenyu" \
      org.opencontainers.image.licenses="Apache-2.0"

ADD target/${APP_NAME}.jar /opt/shenyu-custom-plugin.jar

RUN ls -l
RUN groupadd --system shenyu && \
    useradd --system --gid shenyu --create-home --home-dir /home/shenyu shenyu && \
    chown shenyu:shenyu /opt/shenyu-custom-plugin.jar
USER shenyu
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 CMD ["sh", "-c", "test -s /opt/shenyu-custom-plugin.jar"]
