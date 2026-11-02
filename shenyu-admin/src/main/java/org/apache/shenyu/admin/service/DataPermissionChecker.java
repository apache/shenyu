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

package org.apache.shenyu.admin.service;

/**
 * Verifies that the current dashboard caller may read a persisted selector or rule.
 * <p>The check resolves the persisted namespace of the resource and applies the same
 * data scope as the paged list queries: administrators are unrestricted, while other
 * users need both namespace membership and an explicit {@code data_permission} grant.</p>
 */
public interface DataPermissionChecker {

    /**
     * Require the current caller to be able to read the selector.
     *
     * @param selectorId selector id
     * @param namespaceId persisted namespace id of the selector
     */
    void requireSelectorReadable(String selectorId, String namespaceId);

    /**
     * Require the current caller to be able to read the rule.
     *
     * @param ruleId rule id
     * @param namespaceId persisted namespace id of the rule
     */
    void requireRuleReadable(String ruleId, String namespaceId);
}
