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

package org.apache.shenyu.common.dto.convert.rule;

import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.dto.convert.rule.impl.DubboRuleHandle;
import org.apache.shenyu.common.dto.convert.rule.impl.SofaRuleHandle;
import org.apache.shenyu.common.dto.convert.rule.impl.SpringCloudRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

/**
 * Test cases for RuleHandleFactory.
 */
public final class RuleHandleFactoryTest {

    @Test
    public void testRuleHandleCorrectType() {
        RuleHandle handle = RuleHandleFactory.ruleHandle(PluginEnum.DUBBO.getName(), "", "{\"loadbalance\":\"random\"}");
        assertThat(handle, notNullValue());
        assertThat(handle instanceof DubboRuleHandle, is(true));

        handle = RuleHandleFactory.ruleHandle(PluginEnum.SOFA.getName(), "", "");
        assertThat(handle, notNullValue());
        assertThat(handle instanceof SofaRuleHandle, is(true));

        handle = RuleHandleFactory.ruleHandle(PluginEnum.DIVIDE.getName(), "", "");
        assertThat(handle, notNullValue());
        assertThat(handle instanceof DivideRuleHandle, is(true));

        handle = RuleHandleFactory.ruleHandle(PluginEnum.GRPC.getName(), "", "");
        assertThat(handle, notNullValue());
        assertThat(handle instanceof SpringCloudRuleHandle, is(true));

        handle = RuleHandleFactory.ruleHandle(PluginEnum.SPRING_CLOUD.getName(), "", "");
        assertThat(handle, notNullValue());
        assertThat(handle instanceof SpringCloudRuleHandle, is(true));
    }
}
