/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.bootstrap.plugin.dubbo;

import org.dromara.soul.bootstrap.BaseTest;
import org.dromara.soul.common.dto.convert.rule.DubboRuleHandle;
import org.dromara.soul.common.dto.convert.selector.DubboSelectorHandle;
import org.dromara.soul.web.plugin.dubbo.DubboProxyService;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * The type Dubbo service proxy test.
 *
 * @author xiaoyu(Myth)
 */
public class DubboServiceProxyTest extends BaseTest {

    @Autowired(required = false)
    private DubboProxyService dubboProxyService;

    private DubboSelectorHandle selectorHandle;

    private DubboRuleHandle ruleHandle;

    /**
     * Before.
     */
    @Before
    public void before() {
        selectorHandle = new DubboSelectorHandle();
        selectorHandle.setAppName("local");
        selectorHandle.setRegistry("zookeeper://localhost:2181");
        ruleHandle = new DubboRuleHandle();
    }

    /**
     * Test dubbo entity param.
     */
    @Test
    public void testDubboEntityParam() {
        String json = "{\n" +
                "  \"interfaceName\":\"org.dromara.soul.test.dubbo.api.service.DubboTestService\",\n" +
                "  \"method\":\"insert\",\n" +
                "  \"timeout\":\"50000\",\n" +
                "  \"paramClass\":\"[org.dromara.soul.test.dubbo.api.entity.DubboTest]\",\n" +
                "  \"classParams\":[{\n" +
                "    \"id\":\"xxxx\",\n" +
                "    \"name\":\"xiaoyu\"\n" +
                "  }]\n" +
                "}";

      /*  final Object o = dubboProxyService.genericInvoker(GsonUtils.getInstance().toObjectMap(json), selectorHandle, ruleHandle);
        System.out.println(o.toString());*/
    }

    /**
     * Test dubbo entity string integer param.
     */
    @Test
    public void testDubboEntityStringIntegerParam() {
        String json = "{" +
                "  \"interfaceName\":\"org.dromara.soul.test.dubbo.api.service.DubboTestService\",\n" +
                "  \"method\":\"testEntityStringParam\",\n" +
                "  \"timeout\":\"50000\",\n" +
                "  \"paramClass\":\"[org.dromara.soul.test.dubbo.api.entity.DubboTest]\",\n" +
                "  \"classParams\":[{\n" +
                "    \"id\":\"xxx\",\n" +
                "    \"name\":\"xiaoyu\"\n" +
                "  }],\n" +
                "  \"params\":{" +
                "    \"java.lang.String\":\"666\",\n" +
                "    \"java.lang.Integer\":\"1\"\n" +
                "  }\n" +
                "}";
       /* final Object o = dubboProxyService.genericInvoker(GsonUtils.getInstance().toObjectMap(json), selectorHandle, ruleHandle);
        System.out.println(o.toString());*/
    }

    /**
     * Test dubbo string param.
     */
    @Test
    public void testDubboStringParam() {
        String json = "{\n" +
                "  \"interfaceName\":\"org.dromara.soul.dubbo.api.service.DubboTestService\",\n" +
                "  \"method\":\"findByIdAndName\",\n" +
                "  \"timeout\":\"50000\",\n" +
                "  \"params\":{\n" +
                "    \"java.lang.String\":[\n" +
                "      \"666\",\n" +
                "      \"1\"\n" +
                "    ]\n" +
                "  }\n" +
                "}";
       /* final Object o = dubboProxyService.genericInvoker(GsonUtils.getInstance().toObjectMap(json), selectorHandle, ruleHandle);
        System.out.println(o.toString());*/
    }

    /**
     * Test list entity.
     */
    @Test
    public void testListEntity() {
        String json = "{\n" +
                "    \"interfaceName\": \"org.dromara.soul.test.dubbo.api.service.DubboTestService\", \n" +
                "    \"method\": \"testListEntity\", \n" +
                "    \"timeout\": \"5000\", \n" +
                "    \"paramClass\": [\n" +
                "        \"java.util.List\"\n" +
                "    ], \n" +
                "    \"classParams\": [\n" +
                "        {\n" +
                "            \"id\": \"xxxx\", \n" +
                "            \"name\": \"y\", \n" +
                "            \"class\": \"org.dromara.soul.test.dubbo.api.entity.DubboTest\"\n" +
                "        }, \n" +
                "        {\n" +
                "            \"id\": \"xxx\", \n" +
                "            \"name\": \"y\", \n" +
                "            \"class\": \"org.dromara.soul.test.dubbo.api.entity.DubboTest\"\n" +
                "        }\n" +
                "    ]\n" +
                "}";
      /*  final Object o = dubboProxyService.genericInvoker(GsonUtils.getInstance().toObjectMap(json), selectorHandle, ruleHandle);
        System.out.println(o.toString());*/

    }

}