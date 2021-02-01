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

package org.dromara.soul.common.dto.convert.rule;

import org.dromara.soul.common.dto.convert.rule.impl.DivideRuleHandle;
import org.dromara.soul.common.dto.convert.rule.impl.DubboRuleHandle;
import org.dromara.soul.common.dto.convert.rule.impl.SofaRuleHandle;
import org.dromara.soul.common.dto.convert.rule.impl.SpringCloudRuleHandle;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.Assert.assertThat;

/**
 * Test cases for RuleHandleFactory.
 *
 * @author yiwenlong (wlong.yi@gmail.com)
 */
public final class RuleHandleFactoryTest {

    @Test
    public void testRuleHandleCorrectType() {
        RuleHandle handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.DUBBO, "");
        assertThat(handle, notNullValue());
        assertThat(handle instanceof DubboRuleHandle, is(true));

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.SOFA, "");
        assertThat(handle, notNullValue());
        assertThat(handle instanceof SofaRuleHandle, is(true));

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.HTTP, "");
        assertThat(handle, notNullValue());
        assertThat(handle instanceof DivideRuleHandle, is(true));

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.GRPC, "");
        assertThat(handle, notNullValue());
        assertThat(handle instanceof SpringCloudRuleHandle, is(true));

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.SPRING_CLOUD, "");
        assertThat(handle, notNullValue());
        assertThat(handle instanceof SpringCloudRuleHandle, is(true));

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.MOTAN, "");
        assertThat(handle, notNullValue());
        assertThat(handle instanceof SpringCloudRuleHandle, is(true));

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.WEB_SOCKET, "");
        assertThat(handle, notNullValue());
        assertThat(handle instanceof SpringCloudRuleHandle, is(true));
    }

    @Test
    public void testRuleHandleCorrectTypeNullPath() {
        RuleHandle handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.DUBBO, null);
        assertThat(handle, notNullValue());
        assertThat(handle instanceof DubboRuleHandle, is(true));

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.SOFA, null);
        assertThat(handle, notNullValue());
        assertThat(handle instanceof SofaRuleHandle, is(true));

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.HTTP, null);
        assertThat(handle, notNullValue());
        assertThat(handle instanceof DivideRuleHandle, is(true));

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.GRPC, null);
        assertThat(handle, notNullValue());
        assertThat(handle instanceof SpringCloudRuleHandle, is(true));

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.SPRING_CLOUD, null);
        assertThat(handle, notNullValue());
        assertThat(handle instanceof SpringCloudRuleHandle, is(true));

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.MOTAN, null);
        assertThat(handle, notNullValue());
        assertThat(handle instanceof SpringCloudRuleHandle, is(true));

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.WEB_SOCKET, null);
        assertThat(handle, notNullValue());
        assertThat(handle instanceof SpringCloudRuleHandle, is(true));
    }

    @Test
    public void testRuleHandleNullType() {
        RuleHandle handle = RuleHandleFactory.ruleHandle(null, "");
        assertThat(handle, nullValue());
    }

    @Test
    public void testRuleHandleNullTypeNullPath() {
        RuleHandle handle = RuleHandleFactory.ruleHandle(null, null);
        assertThat(handle, nullValue());
    }
}
