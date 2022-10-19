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

package org.apache.shenyu.sdk.okhttp;

import okhttp3.OkHttpClient;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.ShenyuResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * The type Ok http shenyu sdk client test.
 */
public class OkHttpShenyuSdkClientTest {
    
    /**
     * Test shenyu http client.
     *
     * @throws IOException the io exception
     */
    @Test
    public void testShenyuHttpClient() throws IOException {
        OkHttpShenyuSdkClient okHttpShenyuSdkClient = new OkHttpShenyuSdkClient(new OkHttpClient());
        Map<String, Collection<String>> headerMap = new HashMap<>();
        headerMap.put("header", Arrays.asList("test1", "test2"));
        ShenyuRequest request = ShenyuRequest.create(ShenyuRequest.HttpMethod.GET, "https://shenyu.apache.org",
                headerMap, null, null, null);
        ShenyuResponse response = okHttpShenyuSdkClient.execute(request);
        Assertions.assertNotNull(response);
    }
}
