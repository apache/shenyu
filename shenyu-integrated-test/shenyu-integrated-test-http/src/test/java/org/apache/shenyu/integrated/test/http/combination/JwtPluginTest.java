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

package org.apache.shenyu.integrated.test.http.combination;

import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import org.apache.shenyu.integratedtest.common.AbstractTest;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.junit.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

public final class JwtPluginTest extends AbstractTest {

    @Test
    public void testJwt() throws IOException {
        final String key = "key00000";
        final String testPath = "/http/test/findByUserId?userId=1001";
        final String filterPath = "/http/test/path/1111/name";
        final String token = Jwts.builder().setId("1001").signWith(SignatureAlgorithm.HS256, key.getBytes(StandardCharsets.UTF_8)).compact();
        Map<String, Object> headers = new HashMap<>();

        // send request to filterPath
        Map<String, Object> filterResponse = HttpHelper.INSTANCE.getFromGateway(filterPath, Map.class);
        assertThat(filterResponse.get("userId"), is("1111"));

        // send request with fake jwt
        headers.put("token", "fake.token.me");
        Map<String, Object> errorResponse = HttpHelper.INSTANCE.getFromGateway(testPath, headers, Map.class);
        assertThat(errorResponse.get("message"), is("Illegal authorization"));

        // send request with jwt
        headers.put("token", token);
        Map<String, Object> correctResponse = HttpHelper.INSTANCE.getFromGateway(testPath, headers, Map.class);
        assertThat(correctResponse.get("userId"), is("1001"));
    }
}
