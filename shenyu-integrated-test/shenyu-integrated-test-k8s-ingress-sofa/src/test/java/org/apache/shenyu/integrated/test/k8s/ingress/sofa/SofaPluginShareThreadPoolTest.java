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

package org.apache.shenyu.integrated.test.k8s.ingress.sofa;

import com.google.gson.reflect.TypeToken;
import org.apache.shenyu.integrated.test.k8s.ingress.sofa.dto.SofaTestData;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.hamcrest.core.Is;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;

import static org.hamcrest.MatcherAssert.assertThat;

public class SofaPluginShareThreadPoolTest extends AbstractPluginDataInit {

    private static final HttpHelper HTTP_HELPER = HttpHelper.INSTANCE;

    @BeforeAll
    public static void setup() {
        HTTP_HELPER.setGatewayEndpoint("http://localhost:30095");
    }

    @Test
    public void testHelloWorld() throws IOException {
        SofaTestData response = HttpHelper.INSTANCE.getFromGateway("/sofa/findById?id=1001", new TypeToken<SofaTestData>() { }.getType());
        assertThat(response.getName(), Is.is("hello world shenyu Sofa, findById"));
        assertThat(response.getId(), Is.is("1001"));
    }
}
