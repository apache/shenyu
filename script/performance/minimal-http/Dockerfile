# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements. See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License. You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Build both assemblies with the SAME digest-pinned JAVA_IMAGE and this Dockerfile.
# Supply the extracted distribution through --build-context distribution=PATH.
ARG JAVA_IMAGE
FROM ${JAVA_IMAGE}
ARG MAIN_CLASS
ENV MAIN_CLASS=${MAIN_CLASS}
WORKDIR /opt/shenyu
COPY --from=distribution / /opt/shenyu/
USER 10001:10001
EXPOSE 9195
ENTRYPOINT ["/bin/sh", "-c", "exec java $JAVA_OPTS -cp 'conf:lib/*' \"$MAIN_CLASS\" \"$@\"", "--"]
