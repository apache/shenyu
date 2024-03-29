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

import org.apache.shenyu.plugin.basic.auth.rule.BasicAuthRuleHandle;
import org.apache.shenyu.spi.SPI;

/**
 * Strategy of authentication.
 */
@SPI
public interface BasicAuthAuthenticationStrategy {

    /**
     * HandleJson needs to be parsed into basicAuthRuleHandle in order to
     * specify how to authenticate basicAuth.
     *
     * @param handleJson handleJson from rule
     * @return basicAuthRuleHandle
     */
    BasicAuthRuleHandle parseHandleJson(String handleJson);

    /**
     * authentication.
     *
     * @param basicAuthRuleHandle basicAuthRuleHandle
     * @param authentication authentication
     * @return authentication result
     */
    boolean authenticate(BasicAuthRuleHandle basicAuthRuleHandle, String authentication);

}
