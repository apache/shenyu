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

package org.apache.shenyu.admin.utils;

import org.apache.shenyu.admin.config.properties.JwtProperties;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.spring.SpringBeanUtils;
import org.apache.shiro.subject.Subject;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.context.ConfigurableApplicationContext;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test case for {@link JwtUtils}.
 */
public class JwtUtilsTest {

    public static final String TOKEN = "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJ1c2VyTmFtZSIsImlhdCI6MTYxMTU5MDUwOH0.yAuGpmg1DSYNryZQQA6d66HO87E8eWAFLJVhYscx8K8";

    private static final Long EXPIRED_SECONDS = 86400L;

    private static final String KEY = "jwt-token";

    @BeforeEach
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        JwtProperties jwtProperties = mock(JwtProperties.class);
        when(jwtProperties.getExpiredSeconds()).thenReturn(EXPIRED_SECONDS);
        when(context.getBean(JwtProperties.class)).thenReturn(jwtProperties);
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    @Test
    public void testGetIssuer() {
        assertThat(JwtUtils.getIssuer(TOKEN), is(""));
    }

    @Test
    public void testGetClientId() {
        assertThat(JwtUtils.getClientId(TOKEN), is(""));
    }

    @Test
    public void testGenerateToken() {
        String token = JwtUtils.generateToken("userName", KEY, "clientId");
        assertThat(token, notNullValue());
        assertThat(JwtUtils.getIssuer(token), is("userName"));
        assertThat(JwtUtils.getClientId(token), is("clientId"));
    }

    @Test
    public void testGenerateTokenWithExpireSeconds() {
        Long expireSeconds = 3600L;
        // 1 hour
        String token = JwtUtils.generateToken("userName", KEY, "clientId", expireSeconds);
        assertThat(token, notNullValue());
        assertThat(JwtUtils.getIssuer(token), is("userName"));
        assertThat(JwtUtils.getClientId(token), is("clientId"));
        // Verify token is valid
        assertThat(JwtUtils.verifyToken(token, KEY), is(true));
    }

    @Test
    public void testGenerateTokenWithNullExpireSeconds() {
        String token = JwtUtils.generateToken("userName", KEY, "clientId", null);
        assertThat(token, notNullValue());
        assertThat(JwtUtils.getIssuer(token), is("userName"));
        assertThat(JwtUtils.getClientId(token), is("clientId"));
    }

    @Test
    public void testGenerateTokenWithNullUserName() {
        String token = JwtUtils.generateToken(null, KEY, "clientId");
        assertThat(token, notNullValue());
        assertThat(JwtUtils.getIssuer(token), is(""));
        assertThat(JwtUtils.getClientId(token), is("clientId"));
    }

    @Test
    public void testGenerateTokenWithNullClientId() {
        String token = JwtUtils.generateToken("userName", KEY, null);
        assertThat(token, notNullValue());
        assertThat(JwtUtils.getIssuer(token), is("userName"));
        assertThat(JwtUtils.getClientId(token), is(""));
    }

    @Test
    public void testGenerateTokenWithEmptyKey() {
        String token = JwtUtils.generateToken("userName", "", "clientId");
        assertThat(token, is(""));
    }

    @Test
    public void testGenerateTokenWithNullKey() {
        String token = JwtUtils.generateToken("userName", null, "clientId");
        assertThat(token, is(""));
    }

    @Test
    public void testVerifyTokenWithValidToken() {
        String token = JwtUtils.generateToken("userName", KEY, "clientId");
        assertThat(JwtUtils.verifyToken(token, KEY), is(true));
    }

    @Test
    public void testVerifyTokenWithInvalidToken() {
        assertThat(JwtUtils.verifyToken("invalid.token.here", KEY), is(false));
    }

    @Test
    public void testVerifyTokenWithWrongKey() {
        String token = JwtUtils.generateToken("userName", KEY, "clientId");
        assertThat(JwtUtils.verifyToken(token, "wrong-key"), is(false));
    }

    @Test
    public void testVerifyTokenWithNullToken() {
        assertThrows(NullPointerException.class, () -> JwtUtils.verifyToken(null, KEY));
    }

    @Test
    public void testVerifyTokenWithNullKey() {
        String token = JwtUtils.generateToken("userName", KEY, "clientId");
        assertThrows(IllegalArgumentException.class, () -> JwtUtils.verifyToken(token, null));
    }

    @Test
    public void testVerifyTokenWithEmptyKey() {
        String token = JwtUtils.generateToken("userName", KEY, "clientId");
        assertThrows(IllegalArgumentException.class, () -> JwtUtils.verifyToken(token, ""));
    }

    @Test
    public void testVerifyTokenWithExpiredToken() {
        // Generate token with negative expiration to get an already-expired token
        String token = JwtUtils.generateToken("userName", KEY, "clientId", -1000L);
        assertThat(JwtUtils.verifyToken(token, KEY), is(false));
    }

    @Test
    public void testGetIssuerWithNullToken() {
        assertThrows(NullPointerException.class, () -> JwtUtils.getIssuer(null));
    }

    @Test
    public void testGetIssuerWithEmptyToken() {
        assertThrows(Exception.class, () -> JwtUtils.getIssuer(""));
    }

    @Test
    public void testGetIssuerWithInvalidToken() {
        assertThrows(Exception.class, () -> JwtUtils.getIssuer("invalid.token"));
    }

    @Test
    public void testGetClientIdWithNullToken() {
        assertThrows(NullPointerException.class, () -> JwtUtils.getClientId(null));
    }

    @Test
    public void testGetClientIdWithEmptyToken() {
        assertThrows(Exception.class, () -> JwtUtils.getClientId(""));
    }

    @Test
    public void testGetClientIdWithInvalidToken() {
        assertThrows(Exception.class, () -> JwtUtils.getClientId("invalid.token"));
    }

    @Test
    public void testGetUserInfo() {
        UserInfo mockUserInfo = mock(UserInfo.class);
        Subject mockSubject = mock(Subject.class);
        when(mockSubject.getPrincipal()).thenReturn(mockUserInfo);

        try (MockedStatic<org.apache.shiro.SecurityUtils> securityUtilsMocked = mockStatic(org.apache.shiro.SecurityUtils.class)) {
            securityUtilsMocked.when(org.apache.shiro.SecurityUtils::getSubject).thenReturn(mockSubject);

            UserInfo result = JwtUtils.getUserInfo();
            assertThat(result, is(mockUserInfo));
        }
    }

    @Test
    public void testGetUserInfoWithNullPrincipal() {
        Subject mockSubject = mock(Subject.class);
        when(mockSubject.getPrincipal()).thenReturn(null);

        try (MockedStatic<org.apache.shiro.SecurityUtils> securityUtilsMocked = mockStatic(org.apache.shiro.SecurityUtils.class)) {
            securityUtilsMocked.when(org.apache.shiro.SecurityUtils::getSubject).thenReturn(mockSubject);

            UserInfo result = JwtUtils.getUserInfo();
            assertThat(result, nullValue());
        }
    }
}
