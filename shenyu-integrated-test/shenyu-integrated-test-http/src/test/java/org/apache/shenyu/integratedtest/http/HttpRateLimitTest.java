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
package org.apache.shenyu.integratedtest.http;

import com.google.gson.reflect.TypeToken;
import org.apache.shenyu.integratedtest.http.dto.AdminResponse;
import org.apache.shenyu.integratedtest.http.dto.UserDTO;
import org.apache.shenyu.integratedtest.http.helper.HttpHelper;
import org.junit.Test;

import java.util.concurrent.Future;

import static org.junit.Assert.assertEquals;

public class HttpRateLimitTest extends AbstractTest {

    @Test
    public void testSlidingWindow() throws Exception {
        Future<UserDTO> normalRespFuture = service.submit(() -> HttpHelper.INSTANCE.getFromGateway("/http/test/path/123?name=Tom", UserDTO.class));
        assertEquals("hello world", normalRespFuture.get().getUserName());

        Future<AdminResponse<Object>> rejectedRespFuture = service.submit(() ->
                HttpHelper.INSTANCE.getFromGateway("/http/test/path/123?name=Tom", new TypeToken<AdminResponse<Object>>() {
                }.getType()));
        AdminResponse<Object> dto = rejectedRespFuture.get();
        assertEquals("You have been restricted, please try again later!", dto.getMessage());
    }
}
