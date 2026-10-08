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

# Cloud logging PluginEnum compatibility

The Tencent CLS logging implementation has moved out of the main ShenYu
runtime into the plugin store. The `PluginEnum.LOGGING_TENCENT_CLS`
constant remains in `shenyu-common` as a stable compatibility contract for
store modules, persisted configuration references, and external code that
uses the plugin name or sort value.

The retained API values are:

- Name: `loggingTencentCls`
- Sort: `176`

Current default dependencies, active plugin metadata, menu resources, and
permission rows for the implementation are removed from the main runtime.
Historical upgrade SQL remains unchanged.
