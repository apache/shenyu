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

FROM maven:3.5-jdk-8-alpine
ENV APP_NAME shenyu-integrated-test-upload-plugin
ENV LOCAL_PATH /opt/${APP_NAME}

COPY ./shenyu-custom-plugin ${LOCAL_PATH}/shenyu-custom-plugin
RUN mvn clean install -f ${LOCAL_PATH}/shenyu-custom-plugin/pom.xml  -Dmaven.test.skip=true
RUN mv ${LOCAL_PATH}/custom-plugin/target/custom-plugin-1.0.jar ${LOCAL_PATH}/shenyu-custom-plugin-1.0.jar
RUN chmod 755 ${LOCAL_PATH}/shenyu-custom-plugin-1.0.jar
ADD shenyu-integrated-test-upload-plugin-case/target/${APP_NAME}.jar ${LOCAL_PATH}

WORKDIR ${LOCAL_PATH}
EXPOSE 9195

CMD java -jar ${APP_NAME}.jar
