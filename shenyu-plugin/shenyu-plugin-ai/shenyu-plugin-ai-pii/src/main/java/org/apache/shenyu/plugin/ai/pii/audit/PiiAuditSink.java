/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.plugin.ai.pii.audit;

/**
 * The receiver of audit observations. The default implementation logs them in
 * a structured line, an alternative implementation can forward them to the
 * unified AI invocation auditing once its event vocabulary lands, without
 * changing the plugin: records already carry only type, count, direction,
 * action and rule id.
 */
public interface PiiAuditSink {

    /**
     * Receive one audit observation. The call must not throw on a busy
     * downstream: an audit failure must not decide the fate of the payload.
     *
     * @param record the observation, never carries detected values
     */
    void record(PiiAuditRecord record);
}
