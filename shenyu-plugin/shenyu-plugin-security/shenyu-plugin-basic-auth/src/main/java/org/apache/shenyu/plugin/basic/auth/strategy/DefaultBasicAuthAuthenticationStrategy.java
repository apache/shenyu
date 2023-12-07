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

package org.apache.shenyu.plugin.basic.auth.strategy;

import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.basic.auth.rule.BasicAuthRuleHandle;
import org.apache.shenyu.plugin.basic.auth.rule.DefaultBasicAuthRuleHandle;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Join
public class DefaultBasicAuthAuthenticationStrategy implements BasicAuthAuthenticationStrategy {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultBasicAuthAuthenticationStrategy.class);

    @Override
    public DefaultBasicAuthRuleHandle parseHandleJson(final String handleJson) {
        try {
            return GsonUtils.getInstance().fromJson(handleJson, DefaultBasicAuthRuleHandle.class);
        } catch (Exception exception) {
            LOG.error("Failed to parse json , please check json format", exception);
            return null;
        }
    }

    @Override
    public boolean authenticate(final BasicAuthRuleHandle basicAuthRuleHandle, final String authentication) {
        return authentication.equals(((DefaultBasicAuthRuleHandle) basicAuthRuleHandle).getAuthorization());
    }
}
