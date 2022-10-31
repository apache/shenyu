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

package org.apache.shenyu.plugin.request.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * request plugin handler test.
 */
@ExtendWith(MockitoExtension.class)
public class RequestPluginHandlerTest {

    private RequestPluginHandler requestPluginHandler;

    private RuleData ruleData;

    @BeforeEach
    public void setUp() {
        this.requestPluginHandler = new RequestPluginHandler();
        this.ruleData = new RuleData();
        this.ruleData.setSelectorId("test-selectorId");
        this.ruleData.setName("test-request-plugin");
        this.ruleData.setHandle("{\"header\":{\"addHeaders\":{\"addKey\":\"addValue\"},\"replaceHeaderKeys\":{\"oldKey\":\"newKey\"},"
                + "\"setHeaders\":{\"oldKey\":\"newValue\"},\"removeHeaderKeys\":[\"removeKey\"],\"notEmptyConfig\":true},"
                + "\"parameter\":{\"addParameters\":{\"addKey\":\"addValue\"},\"replaceParameterKeys\":{\"oldKey\":\"newKey\"},"
                + "\"setParameters\":{\"oldKey\":\"newValue\"},\"removeParameterKeys\":[\"removeKey\"],\"notEmptyConfig\":true},"
                + "\"cookie\":{\"addCookies\":{\"addKey\":\"addValue\"},\"replaceCookieKeys\":{\"oldKey\":\"newKey\"},"
                + "\"setCookies\":{\"oldKey\":\"newValue\"},\"removeCookieKeys\":[\"removeKey\"],\"notEmptyConfig\":true},\"emptyConfig\":false}");
    }

    @Test
    public void testHandlerRule() {
        this.requestPluginHandler.handlerRule(this.ruleData);
        assertNotNull(RequestPluginHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(this.ruleData)));
    }

    @Test
    public void testRemoveRule() {
        this.requestPluginHandler.handlerRule(this.ruleData);
        RuleData ruleData = new RuleData();
        ruleData.setSelectorId("test");
        ruleData.setId("test");
        this.requestPluginHandler.removeRule(this.ruleData);
        assertNull(RequestPluginHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(this.ruleData)));
        this.requestPluginHandler.removeRule(ruleData);
        assertNull(RequestPluginHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleData)));
        this.requestPluginHandler.removeRule(null);
    }

    @Test
    public void testPluginNamed() {
        assertEquals(this.requestPluginHandler.pluginNamed(), "request");
    }
}
