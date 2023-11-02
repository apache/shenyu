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

package org.apache.shenyu.sdk.core;

import org.apache.http.HttpStatus;
import org.junit.Assert;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * Test for {@link ShenyuResponse}.
 */
public class ShenyuResponseTest {

    @Test
    public void testShenyuResponse() {
        Map<String, Collection<String>> headerMap = new HashMap<>();
        headerMap.put("header", Arrays.asList("header1", "header2"));
        String body = "{key1:\"value1\"}";
        ShenyuRequest request = ShenyuRequest.create(ShenyuRequest.HttpMethod.GET, "https://shenyu.apache.org",
                headerMap, null, null, null);

        ShenyuResponse response = new ShenyuResponse(
                HttpStatus.SC_OK,
                "success",
                headerMap,
                body,
                request
        );
        Assert.assertNotNull(response);
    }
}
