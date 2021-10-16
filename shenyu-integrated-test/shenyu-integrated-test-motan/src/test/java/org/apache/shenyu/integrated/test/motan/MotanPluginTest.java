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

package org.apache.shenyu.integrated.test.motan;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.MotanDTO;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;
import java.util.Map;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

public class MotanPluginTest extends AbstractPluginDataInit {

    @BeforeClass
    public static void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.MOTAN.getName(), "{\"register\":\"shenyu-zk:2181\"}");
        assertThat(pluginResult, is("success"));
    }

    @Test
    public void testHelloWorld() throws Exception {
        MotanDTO request = new MotanDTO("shenyu");
        Map<String, Object> response = HttpHelper.INSTANCE.postGateway("/motan/hello", request, Map.class);
        assertEquals("hello shenyu", response.get("data"));
    }

}
