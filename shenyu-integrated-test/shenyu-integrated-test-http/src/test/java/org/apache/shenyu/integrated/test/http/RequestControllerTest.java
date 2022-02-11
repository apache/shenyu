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

import okhttp3.Response;
import org.apache.shenyu.integratedtest.common.AbstractTest;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

public final class RequestControllerTest extends AbstractTest {

    @Test
    public void testRequestHeader() throws IOException {
        Map<String, Object> headers = new HashMap<>(2, 1);
        headers.put("header_key1", "header_key1");
        Response response = HttpHelper.INSTANCE.getResponseFromGateway("/http/request/header", headers);
        String body = Objects.requireNonNull(response.body()).string();
        assertTrue(body.contains("response success:"));
    }

    @Test
    public void testRequestParameter() throws IOException {
        String body = HttpHelper.INSTANCE.postGateway("/http/request/parameter?parameter_key1=chuang", String.class);
        assertEquals("response success: {parameter_key1=[chuang]}", body);
    }

    @Test
    public void testRequestCookie() throws IOException {
        String userId = "chuang";
        Map<String, Object> headers = new HashMap<>(2, 1);
        headers.put("Cookie", "userId=" + userId + "");
        Response response = HttpHelper.INSTANCE.getResponseFromGateway("/http/request/cookie", headers);
        String body = Objects.requireNonNull(response.body()).string();
        assertEquals("response success: {userId=[userId=" + userId + "]}", body);
    }
}
