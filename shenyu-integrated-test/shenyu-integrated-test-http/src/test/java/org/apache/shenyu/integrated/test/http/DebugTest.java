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

package org.apache.shenyu.integrated.test.http;

import okhttp3.Headers;
import okhttp3.Response;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.integratedtest.common.AbstractTest;
import org.apache.shenyu.integratedtest.common.dto.OrderDTO;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public class DebugTest extends AbstractTest {
    
    @Test
    public void testHelloWorld() throws Exception {
        OrderDTO user = new OrderDTO("123", "Tom");
        Map<String, Object> requestHeaders = new HashMap<>();
        requestHeaders.put("debug", "enable");
        Response response = HttpHelper.INSTANCE.post("/http/order/save", requestHeaders, user);
        assertNotNull(response);
        final Headers responseHeaders = response.headers();
        final String metaData = responseHeaders.get(Constants.META_DATA);
        final String selector = responseHeaders.get(Constants.SELECTOR);
        final String rule = responseHeaders.get(Constants.RULE);
        assertFalse(StringUtils.isAnyBlank(metaData, selector, rule));
    }
}
