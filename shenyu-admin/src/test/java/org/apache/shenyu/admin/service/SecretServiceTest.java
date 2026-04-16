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

package org.apache.shenyu.admin.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.admin.config.properties.SecretProperties;
import org.apache.shenyu.admin.service.impl.SecretServiceImpl;
import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.Base64;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;

/**
 * Test for {@link SecretServiceImpl}.
 */
public class SecretServiceTest {

    private static final String TEST_AES_KEY = "2095132720951327";

    private static final String TEST_AES_IV = "6075877187097700";

    @Test
    public void infoShouldNotExposeConfiguredSecrets() throws Exception {
        SecretProperties secretProperties = new SecretProperties();
        secretProperties.setKey(TEST_AES_KEY);
        secretProperties.setIv(TEST_AES_IV);

        SecretService secretService = new SecretServiceImpl();
        String encoded = secretService.info();
        String decoded = new String(Base64.getDecoder().decode(encoded), StandardCharsets.UTF_8);
        @SuppressWarnings("unchecked")
        Map<String, String> secretInfo = new ObjectMapper().readValue(decoded, Map.class);

        assertNotEquals(TEST_AES_KEY, secretInfo.get("key"));
        assertNotEquals(TEST_AES_IV, secretInfo.get("iv"));
        assertEquals("", secretInfo.get("key"));
        assertEquals("", secretInfo.get("iv"));
    }
}
