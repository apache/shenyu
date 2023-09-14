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

package org.apache.shenyu.integrated.test.k8s.ingress.apache.dubbo;

import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.DubboTest;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ApacheDubboPluginTest extends AbstractPluginDataInit {

    private static final HttpHelper HTTP_HELPER = HttpHelper.INSTANCE;

    @BeforeAll
    public static void setup() {
        HTTP_HELPER.setGatewayEndpoint("http://localhost:30095");
    }

    @Test
    public void testFindById() throws IOException {
        DubboTest dubboTest = HttpHelper.INSTANCE.getFromGateway("/findById?id=1", DubboTest.class);
        assertEquals("hello world shenyu Apache, findById", dubboTest.getName());
        assertEquals("1", dubboTest.getId());
    }

    @Test
    public void testFindAll() throws IOException {
        DubboTest dubboTest = HttpHelper.INSTANCE.getFromGateway("/findAll", DubboTest.class);
        assertEquals("hello world shenyu Apache, findAll", dubboTest.getName());
    }
}
