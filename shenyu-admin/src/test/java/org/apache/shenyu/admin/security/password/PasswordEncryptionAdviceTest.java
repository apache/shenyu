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

package org.apache.shenyu.admin.security.password;

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import org.apache.shenyu.admin.controller.DashboardUserController;
import org.apache.shenyu.admin.controller.PlatformController;
import org.apache.shenyu.admin.exception.ExceptionHandlers;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.dto.DashboardUserModifyPasswordDTO;
import org.apache.shenyu.admin.model.dto.DashboardUserDTO;
import org.apache.shenyu.admin.model.vo.LoginDashboardUserVO;
import org.apache.shenyu.admin.service.DashboardUserService;
import org.apache.shenyu.admin.service.EnumService;
import org.apache.shenyu.admin.service.PasswordHashService;
import org.apache.shenyu.admin.service.SecretService;
import org.apache.shenyu.admin.service.impl.SecretServiceImpl;
import org.apache.shenyu.admin.config.properties.SecretProperties;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shiro.mgt.DefaultSecurityManager;
import org.apache.shiro.subject.SimplePrincipalCollection;
import org.apache.shiro.subject.Subject;
import org.apache.shiro.util.ThreadContext;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.validation.beanvalidation.LocalValidatorFactoryBean;
import org.springframework.web.servlet.HandlerInterceptor;
import org.springframework.web.servlet.config.annotation.InterceptorRegistry;

import java.net.URI;
import java.util.Base64;
import java.util.List;
import java.util.Objects;

import static org.apache.shenyu.admin.security.password.PasswordEnvelopeFixtures.MAPPER;
import static org.apache.shenyu.admin.security.password.PasswordEnvelopeFixtures.encrypt;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.head;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.options;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Original-controller MockMvc coverage for conversion, validation and strict compatibility policy.
 */
class PasswordEncryptionAdviceTest {

    private final DashboardUserService users = mock(DashboardUserService.class);

    private final PasswordEncryptionProperties properties = new PasswordEncryptionProperties();

    private LocalValidatorFactoryBean validator;

    private PasswordEncryptionService service;

    private MockMvc mvc;

    @BeforeEach
    void setUp() {
        validator = new LocalValidatorFactoryBean();
        validator.afterPropertiesSet();
        service = new PasswordEncryptionService(properties, new SecretProperties(), validator);
        SecretService secrets = new SecretServiceImpl();
        PlatformController platform = new PlatformController(users, mock(EnumService.class), secrets);
        DashboardUserController dashboard = new DashboardUserController(users, mock(PasswordHashService.class));
        InterceptorRegistry registry = new InterceptorRegistry();
        new PasswordEncryptionWebConfiguration(properties).addInterceptors(registry);
        List<HandlerInterceptor> interceptors = ReflectionTestUtils.invokeMethod(registry, "getInterceptors");
        mvc = MockMvcBuilders.standaloneSetup(platform, dashboard)
                .setValidator(validator)
                .setControllerAdvice(new PasswordEncryptionAdvice(service), new ExceptionHandlers(null))
                .addInterceptors(interceptors.toArray(new HandlerInterceptor[0]))
                .build();
        SimplePrincipalCollection principals = new SimplePrincipalCollection(UserInfo.builder().userId("2").userName("test-user").build(), "real");
        ThreadContext.bind(new DefaultSecurityManager());
        ThreadContext.bind(new Subject.Builder().principals(principals).buildSubject());
    }

    @AfterEach
    void tearDown() {
        ThreadContext.remove();
        validator.close();
    }

    @Test
    void augmentsOnlySecretInfoAndPreservesOriginalSecretServiceContract() throws Exception {
        MvcResult result = mvc.perform(get("/platform/secretInfo"))
                .andExpect(status().isOk()).andExpect(jsonPath("$.code").value(200))
                .andExpect(header().string("Cache-Control", "no-store")).andReturn();
        JsonNode response = MAPPER.readTree(result.getResponse().getContentAsByteArray());
        JsonNode info = MAPPER.readTree(Base64.getDecoder().decode(response.get("data").textValue()));
        assertEquals("", info.get("key").textValue());
        assertEquals("", info.get("iv").textValue());
        assertEquals(1, info.get("encryption").get("version").intValue());
        JsonNode original = MAPPER.readTree(Base64.getDecoder().decode(new SecretServiceImpl().info()));
        assertFalse(original.has("encryption"));
        assertEquals("", original.get("key").textValue());
    }

    @Test
    void passesDecryptedLoginDataToTheUnchangedController() throws Exception {
        ObjectNode login = MAPPER.createObjectNode().put("userName", "test-user").put("password", "中文very-long-ldap-password".repeat(100));
        LoginDashboardUserVO user = new LoginDashboardUserVO();
        user.setEnabled(true);
        user.setUserName("test-user");
        when(users.login(eq("test-user"), eq(login.get("password").textValue()), isNull())).thenReturn(user);
        mvc.perform(post("/platform/login").contentType(MediaType.APPLICATION_JSON).content(encrypt(service, "login", "", login)))
                .andExpect(status().isOk()).andExpect(jsonPath("$.code").value(200));
        verify(users).login("test-user", login.get("password").textValue(), null);
    }

    @Test
    void decryptsPasswordChangeBeforeTheExistingDtoAndControllerRun() throws Exception {
        ObjectNode change = change("New!Password2", "old-password");
        when(users.modifyPassword(any())).thenReturn(1);
        mvc.perform(put("/dashboardUser/modify-password/2").contentType(MediaType.APPLICATION_JSON)
                        .content(encrypt(service, "change-password", "2", change)))
                .andExpect(status().isOk()).andExpect(jsonPath("$.code").value(200)).andExpect(jsonPath("$.data").value(1));
        ArgumentCaptor<DashboardUserModifyPasswordDTO> dto = ArgumentCaptor.forClass(DashboardUserModifyPasswordDTO.class);
        verify(users).modifyPassword(dto.capture());
        assertEquals("2", dto.getValue().getId());
        assertEquals("New!Password2", dto.getValue().getPassword());
        assertEquals("old-password", dto.getValue().getOldPassword());
    }

    @Test
    void usesTheDecodedPathIdForAdditionalAuthenticatedData() throws Exception {
        SimplePrincipalCollection principals = new SimplePrincipalCollection(UserInfo.builder().userId("用户2").userName("test-user").build(), "real");
        ThreadContext.bind(new Subject.Builder().principals(principals).buildSubject());
        when(users.modifyPassword(any())).thenReturn(1);
        mvc.perform(put(URI.create("/dashboardUser/modify-password/%E7%94%A8%E6%88%B72")).contentType(MediaType.APPLICATION_JSON)
                        .content(encrypt(service, "change-password", "用户2", change("New!Password2", "old-password"))))
                .andExpect(jsonPath("$.code").value(200));
        verify(users).modifyPassword(any());
    }

    @Test
    void keepsWrongOldPasswordFailureInTheOriginalServiceFlow() throws Exception {
        when(users.modifyPassword(any())).thenThrow(new ShenyuException("old password is error"));
        mvc.perform(put("/dashboardUser/modify-password/2").contentType(MediaType.APPLICATION_JSON)
                        .content(encrypt(service, "change-password", "2", change("New!Password2", "wrong-old-password"))))
                .andExpect(status().isOk()).andExpect(jsonPath("$.code").value(500)).andExpect(jsonPath("$.message").value("old password is error"));
        ArgumentCaptor<DashboardUserModifyPasswordDTO> dto = ArgumentCaptor.forClass(DashboardUserModifyPasswordDTO.class);
        verify(users).modifyPassword(dto.capture());
        assertEquals("wrong-old-password", dto.getValue().getOldPassword());
    }

    @Test
    void rejectsWeakNewPasswordAndEmptyLoginWithoutLoggingRejectedPlaintext() throws Exception {
        Logger root = (Logger) LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME);
        ListAppender<ILoggingEvent> captured = new ListAppender<>();
        captured.start();
        root.addAppender(captured);
        try {
            mvc.perform(put("/dashboardUser/modify-password/2").contentType(MediaType.APPLICATION_JSON)
                            .content(encrypt(service, "change-password", "2", change("sensitive-weak-plain-text", "sensitive-old-password"))))
                    .andExpect(jsonPath("$.code").value(500)).andExpect(jsonPath("$.message").value("Invalid password request"));
            ObjectNode login = MAPPER.createObjectNode().put("userName", "test-user").put("password", "");
            mvc.perform(post("/platform/login").contentType(MediaType.APPLICATION_JSON).content(encrypt(service, "login", "", login)))
                    .andExpect(jsonPath("$.code").value(500)).andExpect(jsonPath("$.message").value("Invalid password request"));
            for (ILoggingEvent event : captured.list) {
                assertFalse(event.getFormattedMessage().contains("sensitive-weak-plain-text"));
                assertFalse(event.getFormattedMessage().contains("sensitive-old-password"));
                assertFalse(Objects.nonNull(event.getThrowableProxy()) && event.getThrowableProxy().getMessage().contains("sensitive-weak-plain-text"));
            }
            verifyNoInteractions(users);
        } finally {
            root.detachAppender(captured);
            captured.stop();
        }
    }

    @Test
    void preservesUnrelatedDtoValidationMessagesWithoutRenderingOrLoggingRejectedValues() throws Exception {
        DashboardUserDTO dto = new DashboardUserDTO();
        dto.setUserName("new-user");
        dto.setEnabled(true);
        dto.setPassword("sensitive-rejected-value");
        String defaultMessage = validator.validate(dto).iterator().next().getMessage();
        Logger root = (Logger) LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME);
        ListAppender<ILoggingEvent> captured = new ListAppender<>();
        captured.start();
        root.addAppender(captured);
        try {
            mvc.perform(post("/dashboardUser").contentType(MediaType.APPLICATION_JSON).content(MAPPER.writeValueAsBytes(dto)))
                    .andExpect(status().isOk()).andExpect(jsonPath("$.code").value(500))
                    .andExpect(jsonPath("$.message").value("Request error! invalid argument [password: " + defaultMessage + "]"));
            mvc.perform(post("/dashboardUser").contentType(MediaType.APPLICATION_JSON).content("{ malformed-sensitive-json-value"))
                    .andExpect(status().isOk()).andExpect(jsonPath("$.code").value(500))
                    .andExpect(jsonPath("$.message").value("The system is busy, please try again later"));
            for (ILoggingEvent event : captured.list) {
                assertFalse(event.getFormattedMessage().contains(dto.getPassword()));
                assertFalse(event.getFormattedMessage().contains("sensitive-json-value"));
                assertFalse(Objects.nonNull(event.getThrowableProxy()) && event.getThrowableProxy().getMessage().contains(dto.getPassword()));
            }
            verifyNoInteractions(users);
        } finally {
            root.detachAppender(captured);
            captured.stop();
        }
    }

    @Test
    void strictModeRejectsPlaintextPostPutAndLegacyGetOrHeadButAllowsOptions() throws Exception {
        properties.setRequired(true);
        mvc.perform(post("/platform/login").contentType(MediaType.APPLICATION_JSON)
                        .content(MAPPER.writeValueAsBytes(MAPPER.createObjectNode().put("userName", "test-user").put("password", "plain"))))
                .andExpect(jsonPath("$.code").value(500));
        mvc.perform(put("/dashboardUser/modify-password/2").contentType(MediaType.APPLICATION_JSON)
                        .content(MAPPER.writeValueAsBytes(change("New!Password2", "plain-old"))))
                .andExpect(jsonPath("$.code").value(500));
        mvc.perform(get("/platform/login").param("userName", "test-user").param("password", "plain"))
                .andExpect(jsonPath("$.code").value(500));
        MvcResult head = mvc.perform(head("/platform/login").param("userName", "test-user").param("password", "plain")).andReturn();
        assertInstanceOf(InvalidPasswordRequestException.class, head.getResolvedException());
        mvc.perform(options("/platform/login")).andExpect(status().isOk());
        verifyNoInteractions(users);
    }

    @Test
    void compatibilityModeKeepsLegacyPostAndGetAvailable() throws Exception {
        ObjectNode login = MAPPER.createObjectNode().put("userName", "test-user").put("password", "plain");
        mvc.perform(post("/platform/login").contentType(MediaType.APPLICATION_JSON).content(MAPPER.writeValueAsBytes(login)))
                .andExpect(status().isOk());
        mvc.perform(get("/platform/login").param("userName", "other-user").param("password", "legacy-plain"))
                .andExpect(status().isOk());
        verify(users).login("test-user", "plain", null);
        verify(users).login("other-user", "legacy-plain", null);
    }

    @Test
    void rejectsPathBindingMismatchAndOversizedOrMalformedBodiesBeforeController() throws Exception {
        mvc.perform(put("/dashboardUser/modify-password/2").contentType(MediaType.APPLICATION_JSON)
                        .content(encrypt(service, "change-password", "3", change("New!Password2", "plain-old"))))
                .andExpect(jsonPath("$.code").value(500));
        mvc.perform(post("/platform/login").contentType(MediaType.APPLICATION_JSON).content(new byte[PasswordEncryptionService.MAX_BODY_BYTES + 1]))
                .andExpect(jsonPath("$.code").value(500));
        mvc.perform(post("/platform/login").contentType(MediaType.APPLICATION_JSON).content("{ malformed"))
                .andExpect(jsonPath("$.code").value(500));
        verify(users, never()).modifyPassword(any());
        verify(users, never()).login(any(), any(), any());
    }

    private ObjectNode change(final String password, final String oldPassword) {
        return MAPPER.createObjectNode().put("userName", "test-user").put("password", password).put("oldPassword", oldPassword);
    }
}
