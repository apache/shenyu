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

package org.apache.shenyu.admin.validation.validator;

import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.common.config.contract.ConfigurationContractRegistry;
import org.apache.shenyu.common.config.contract.ConfigurationScope;
import org.apache.shenyu.common.config.contract.ConfigurationValidation;
import org.apache.shenyu.common.config.contract.ConfigurationViolation;

/**
 * Validates rewrite rule handles before Admin stores them.
 */
public final class RewriteRuleHandleValidator {

    private RewriteRuleHandleValidator() {
    }

    /**
     * Validate a rewrite rule handle, rejecting the rule before it is persisted when the gateway
     * could never apply it.
     *
     * @param handle the rule handle supplied to Admin
     */
    public static void validate(final String handle) {
        ConfigurationValidation result = ConfigurationContractRegistry.shared().validate("rewrite", ConfigurationScope.RULE, "1", handle);
        if (result.status() == ConfigurationValidation.Status.INVALID) {
            ConfigurationViolation problem = result.violations().get(0);
            if (problem.code() == ConfigurationViolation.Code.SEMANTIC_INVALID) {
                throw new ShenyuAdminException("rewrite rule handle can not be applied, "
                        + ("regex".equals(problem.path()) ? "invalid regular expression" : "invalid replacement template"));
            }
            throw new ShenyuAdminException("rewrite rule handle must be a valid JSON object");
        }
    }
}
