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

package org.apache.shenyu.plugin.ai.proxy.enhanced.cache;

/**
 * Explicit server-owned authorization to reuse a response within an isolation scope.
 * This must not be constructed from unverified request headers or diagnostic context.
 * A resolver must authorize the current request before issuing this scope.
 *
 * @param tenant authenticated tenant
 * @param subject authenticated subject
 * @param accessScope approved data-access partition
 * @param policyRevision authorization and routing policy revision
 * @param promptVersion deployed prompt revision
 * @param selectorId matched AI selector
 * @param ruleId matched AI rule
 */
public record AiResponseCacheScope(String tenant, String subject, String accessScope,
                                   String policyRevision, String promptVersion, String selectorId, String ruleId) {

    public static final String REACTOR_KEY = AiResponseCacheScope.class.getName();

    @Override
    public String toString() {
        return "AiResponseCacheScope[redacted]";
    }
}
