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

import com.google.gson.JsonSyntaxException;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.common.dto.convert.rule.RewriteHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.RewriteHandleUtils;

import java.util.Objects;

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
        if (StringUtils.isBlank(handle)) {
            // an empty handle means the rule does not rewrite, which the plugin tolerates as well
            return;
        }
        final RewriteHandle rewriteHandle;
        try {
            rewriteHandle = GsonUtils.getInstance().fromJson(handle, RewriteHandle.class);
        } catch (JsonSyntaxException ex) {
            throw new ShenyuAdminException("rewrite rule handle must be a valid JSON object");
        }
        if (Objects.isNull(rewriteHandle)) {
            throw new ShenyuAdminException("rewrite rule handle must be a valid JSON object");
        }
        final String problem = RewriteHandleUtils.applicabilityProblem(rewriteHandle.getRegex(), rewriteHandle.getReplace());
        if (Objects.nonNull(problem)) {
            throw new ShenyuAdminException("rewrite rule handle can not be applied, " + problem);
        }
    }
}
