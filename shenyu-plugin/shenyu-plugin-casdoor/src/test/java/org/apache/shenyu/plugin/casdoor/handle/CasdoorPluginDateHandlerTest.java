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

package org.apache.shenyu.plugin.casdoor.handle;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.Singleton;
import org.casbin.casdoor.service.CasdoorAuthService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class CasdoorPluginDateHandlerTest {

    private CasdoorPluginDateHandler casdoorPluginDateHandlerTest;

    @BeforeEach
    public void setup() {
        casdoorPluginDateHandlerTest = new CasdoorPluginDateHandler();
    }

    @Test
    public void handlerPlugin() {
        final PluginData pluginData = new PluginData("pluginId", "pluginName", "{\"organization-name\":\"test\",\"application-name\":\"app-test\",\"endpoint\":\"http://localhost:8000\",\"client_secrect\":\"a4209d412a33a842b7a9c05a3446e623cbb7262d\",\"client_id\":\"6e3a84154e73d1fb156a\",\"certificate\":\"-----BEGIN CERTIFICATE-----\\n\"}", "0", false, null);
        casdoorPluginDateHandlerTest.handlerPlugin(pluginData);
        CasdoorAuthService casdoorAuthService = Singleton.INST.get(CasdoorAuthService.class);
        String redirect = "http://localhost:9195/http/hi";
        assertEquals(casdoorAuthService.getSigninUrl(redirect), "http://localhost:8000/login/oauth/authorize?client_id=6e3a84154e73d1fb156a&response_type=code&redirect_uri=http%3A%2F%2Flocalhost%3A9195%2Fhttp%2Fhi&scope=read&state=app-test");
    }

    @Test
    public void testPluginNamed() {
        final String result = casdoorPluginDateHandlerTest.pluginNamed();
        assertEquals(PluginEnum.CASDOOR.getName(), result);
    }
}
