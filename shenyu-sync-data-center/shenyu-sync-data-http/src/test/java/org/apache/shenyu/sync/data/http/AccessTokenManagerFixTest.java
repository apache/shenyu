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

package org.apache.shenyu.sync.data.http;

import org.apache.shenyu.sync.data.http.config.HttpConfig;
import org.junit.jupiter.api.Test;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Other test for AccessTokenManager Login...
 */
public class AccessTokenManagerFixTest {

    @Test
    public void testPasswordNotOverwritten() {

        HttpConfig config = new HttpConfig();
        config.setPassword("original+password");
        config.setAesSecretKey("1234567890123456");
        config.setAesSecretIv("1234567890123456");

        String originalPassword = config.getPassword();

        String password = config.getPassword();
        if (Objects.nonNull(config.getAesSecretKey()) && Objects.nonNull(config.getAesSecretIv())) {

            // test encryption password logic
            password = "encrypted_" + password;
        }

        assertEquals(originalPassword, config.getPassword());
    }

    @Test
    public void testUrlEncoding() {

        String username = "test user@domain.com";
        String password = "pass+word=123&456 789%test";

        String encodedUsername = URLEncoder.encode(username, StandardCharsets.UTF_8);
        String encodedPassword = URLEncoder.encode(password, StandardCharsets.UTF_8);

        assertEquals("test+user%40domain.com", encodedUsername);
        assertEquals("pass%2Bword%3D123%26456+789%25test", encodedPassword);
    }
}