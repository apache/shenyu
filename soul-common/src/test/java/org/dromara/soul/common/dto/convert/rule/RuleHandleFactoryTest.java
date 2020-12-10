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
import org.junit.Assert;
import org.junit.Test;

/**
 * Test cases for RuleHandleFactory.
 *
 * @author yiwenlong (wlong.yi@gmail.com)
 */
public final class RuleHandleFactoryTest {

    @Test
    public void testRuleHandleCorrectType() {
        RuleHandle handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.DUBBO, "");
        Assert.assertNotNull(handle);
        Assert.assertTrue(handle instanceof DubboRuleHandle);

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.SOFA, "");
        Assert.assertNotNull(handle);
        Assert.assertTrue(handle instanceof SofaRuleHandle);

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.HTTP, "");
        Assert.assertNotNull(handle);
        Assert.assertTrue(handle instanceof DivideRuleHandle);

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.GRPC, "");
        Assert.assertNotNull(handle);
        Assert.assertTrue(handle instanceof SpringCloudRuleHandle);

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.SPRING_CLOUD, "");
        Assert.assertNotNull(handle);
        Assert.assertTrue(handle instanceof SpringCloudRuleHandle);

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.MOTAN, "");
        Assert.assertNotNull(handle);
        Assert.assertTrue(handle instanceof SpringCloudRuleHandle);

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.WEB_SOCKET, "");
        Assert.assertNotNull(handle);
        Assert.assertTrue(handle instanceof SpringCloudRuleHandle);
    }

    @Test
    public void testRuleHandleCorrectTypeNullPath() {
        RuleHandle handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.DUBBO, null);
        Assert.assertNotNull(handle);
        Assert.assertTrue(handle instanceof DubboRuleHandle);

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.SOFA, null);
        Assert.assertNotNull(handle);
        Assert.assertTrue(handle instanceof SofaRuleHandle);

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.HTTP, null);
        Assert.assertNotNull(handle);
        Assert.assertTrue(handle instanceof DivideRuleHandle);

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.GRPC, null);
        Assert.assertNotNull(handle);
        Assert.assertTrue(handle instanceof SpringCloudRuleHandle);

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.SPRING_CLOUD, null);
        Assert.assertNotNull(handle);
        Assert.assertTrue(handle instanceof SpringCloudRuleHandle);

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.MOTAN, null);
        Assert.assertNotNull(handle);
        Assert.assertTrue(handle instanceof SpringCloudRuleHandle);

        handle = RuleHandleFactory.ruleHandle(RpcTypeEnum.WEB_SOCKET, null);
        Assert.assertNotNull(handle);
        Assert.assertTrue(handle instanceof SpringCloudRuleHandle);
    }

    @Test
    public void testRuleHandleNullType() {
        RuleHandle handle = RuleHandleFactory.ruleHandle(null, "");
        Assert.assertNull(handle);
    }

    @Test
    public void testRuleHandleNullTypeNullPath() {
        RuleHandle handle = RuleHandleFactory.ruleHandle(null, null);
        Assert.assertNull(handle);
    }
}
