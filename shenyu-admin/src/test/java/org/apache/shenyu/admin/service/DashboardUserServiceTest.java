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

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import org.apache.shenyu.admin.config.properties.JwtProperties;
import org.apache.shenyu.admin.config.properties.LdapProperties;
import org.apache.shenyu.admin.config.properties.SecretProperties;
import org.apache.shenyu.admin.exception.ValidFailException;
import org.apache.shenyu.admin.mapper.DashboardUserMapper;
import org.apache.shenyu.admin.mapper.RoleMapper;
import org.apache.shenyu.admin.mapper.UserRoleMapper;
import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.dto.DashboardUserDTO;
import org.apache.shenyu.admin.model.entity.DashboardUserDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.DashboardUserQuery;
import org.apache.shenyu.admin.model.vo.DashboardUserVO;
import org.apache.shenyu.admin.model.vo.LoginDashboardUserVO;
import org.apache.shenyu.admin.model.vo.NamespaceUserRelVO;
import org.apache.shenyu.admin.security.SecurityAuditLog;
import org.apache.shenyu.admin.service.impl.DashboardUserServiceImpl;
import org.apache.shenyu.admin.service.publish.UserEventPublisher;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.admin.utils.JwtUtils;
import org.apache.shenyu.common.utils.AesUtils;
import org.apache.shenyu.common.utils.DigestUtils;
import org.apache.shenyu.common.utils.ListUtil;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.ldap.core.LdapTemplate;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.argThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * test cases for DashboardUserService.
 */
@ExtendWith(MockitoExtension.class)
public final class DashboardUserServiceTest {

    public static final String TEST_ID = "testId";

    public static final String TEST_USER_NAME = "userName";

    public static final String TEST_PASSWORD = "password";

    private static final String TEST_AES_KEY = "2095132720951327";

    private static final String TEST_AES_IV = "6075877187097700";

    private static final String TEST_JWT_SECRET_KEY = "testSecretKey12345678901234567890";

    @InjectMocks
    private DashboardUserServiceImpl dashboardUserService;

    @Mock
    private DashboardUserMapper dashboardUserMapper;

    @Mock
    private UserRoleMapper userRoleMapper;

    @Mock
    private RoleMapper roleMapper;

    @Mock
    private UserEventPublisher publisher;

    @Mock
    private JwtProperties jwtProperties;

    @Mock
    private LdapTemplate ldapTemplate;

    @Mock
    private SecretProperties secretProperties;

    @Mock
    private NamespaceUserService namespaceUserService;

    @Mock
    private PasswordHashService passwordHashService;

    @AfterEach
    public void tearDown() {
        MDC.clear();
        SecurityAuditLog.resetRateLimiter();
        RequestContextHolder.resetRequestAttributes();
    }

    @Test
    public void testCreateOrUpdate() {
        SessionUtil.setLocalVisitor(UserInfo.builder().userId("1").userName("admin").build());
        final DashboardUserDTO dashboardUserDTO = DashboardUserDTO.builder()
                .userName(TEST_USER_NAME).password(TEST_PASSWORD).roles(Collections.singletonList("1"))
                .build();
        given(passwordHashService.isBcryptHash(TEST_PASSWORD)).willReturn(false);
        given(passwordHashService.encode(TEST_PASSWORD)).willReturn("bcrypt-password");
        given(dashboardUserMapper.insertSelective(any(DashboardUserDO.class))).willReturn(1);
        given(namespaceUserService.create(any(), any())).willReturn(new NamespaceUserRelVO());
        assertEquals(1, dashboardUserService.createOrUpdate(dashboardUserDTO));
        verify(dashboardUserMapper).insertSelective(any(DashboardUserDO.class));

        dashboardUserDTO.setId(TEST_ID);
        given(dashboardUserMapper.selectById(TEST_ID)).willReturn(createDashboardUserDO());
        given(dashboardUserMapper.updateSelective(any(DashboardUserDO.class))).willReturn(2);
        assertEquals(2, dashboardUserService.createOrUpdate(dashboardUserDTO));
        verify(dashboardUserMapper).updateSelective(any(DashboardUserDO.class));
    }

    @Test
    public void testDelete() {
        List<String> deleteIds = Stream.of("1", "2").collect(Collectors.toList());
        given(userRoleMapper.deleteByUserIdList(deleteIds)).willReturn(deleteIds.size());
//        given(dataPermissionMapper.deleteByUserIdList(deleteIds)).willReturn(deleteIds.size());
        given(dashboardUserMapper.selectByIds(new HashSet<>(deleteIds))).willReturn(ListUtil.map(deleteIds, this::mockUserById));
        given(dashboardUserMapper.deleteByIdList(deleteIds)).willReturn(deleteIds.size());
        assertEquals(deleteIds.size(), dashboardUserService.delete(new HashSet<>(deleteIds)));
    }

    private DashboardUserDO mockUserById(final String id) {
        return DashboardUserDO.builder()
                .id(id)
                .userName("mockUser" + id)
                .enabled(true)
                .role(1)
                .dateCreated(new Timestamp(System.currentTimeMillis()))
                .dateUpdated(new Timestamp(System.currentTimeMillis()))
                .build();
    }

    @Test
    public void testFindById() {
        DashboardUserDO dashboardUserDO = createDashboardUserDO();
        given(dashboardUserMapper.selectById(eq(TEST_ID))).willReturn(dashboardUserDO);

        DashboardUserVO dashboardUserVO = dashboardUserService.findById(TEST_ID);
        assertEquals(TEST_ID, dashboardUserVO.getId());
        verify(dashboardUserMapper).selectById(eq(TEST_ID));
    }

    @Test
    public void testFindByUsername() {
        DashboardUserDO dashboardUserDO = createDashboardUserDO();
        given(dashboardUserMapper.selectByUserName(eq(TEST_USER_NAME))).willReturn(dashboardUserDO);

        DashboardUserVO dashboardUserVO = dashboardUserService.findByUserName(TEST_USER_NAME);
        assertEquals(TEST_ID, dashboardUserVO.getId());
        assertEquals(TEST_USER_NAME, dashboardUserVO.getUserName());
        assertEquals(TEST_PASSWORD, dashboardUserVO.getPassword());
        verify(dashboardUserMapper).selectByUserName(eq(TEST_USER_NAME));
    }

    @Test
    public void testListByPage() {
        DashboardUserQuery dashboardUserQuery = new DashboardUserQuery();
        dashboardUserQuery.setUserName(TEST_USER_NAME);
        PageParameter pageParameter = new PageParameter();
        dashboardUserQuery.setPageParameter(pageParameter);

        DashboardUserDO dashboardUserDO = createDashboardUserDO();
        given(dashboardUserMapper.selectByQuery(eq(dashboardUserQuery))).willReturn(Collections.singletonList(dashboardUserDO));

        CommonPager<DashboardUserVO> commonPager = dashboardUserService.listByPage(dashboardUserQuery);
        assertThat(commonPager.getDataList()).isNotNull().isNotEmpty();
        assertEquals(1, commonPager.getDataList().size());
        assertEquals(TEST_ID, commonPager.getDataList().get(0).getId());
        verify(dashboardUserMapper).selectByQuery(eq(dashboardUserQuery));
    }

    @Test
    public void testLogin() {
        ReflectionTestUtils.setField(dashboardUserService, "jwtProperties", jwtProperties);
        ReflectionTestUtils.setField(dashboardUserService, "secretProperties", secretProperties);
        given(jwtProperties.getSecretKey()).willReturn(TEST_JWT_SECRET_KEY);
        given(jwtProperties.getExpiredSeconds()).willReturn(86400000L);
        DashboardUserDO dashboardUserDO = createDashboardUserDO();

        when(dashboardUserMapper.selectByUserName(eq(TEST_USER_NAME))).thenReturn(dashboardUserDO);
        given(ldapTemplate.authenticate(anyString(), anyString(), anyString())).willReturn(true);
        // test loginByLdap
        LdapProperties ldapProperties = new LdapProperties();
        ldapProperties.setBaseDn("test");
        ReflectionTestUtils.setField(dashboardUserService, "ldapProperties", ldapProperties);
        ReflectionTestUtils.setField(dashboardUserService, "ldapTemplate", ldapTemplate);
        given(passwordHashService.matches(TEST_PASSWORD, TEST_PASSWORD)).willReturn(true);
        LoginDashboardUserVO loginDashboardUserVO = dashboardUserService.login(TEST_USER_NAME, TEST_PASSWORD, null);
        assertEquals(TEST_USER_NAME, loginDashboardUserVO.getUserName());
        assertEquals(TEST_PASSWORD, loginDashboardUserVO.getPassword());
        assertTrue(JwtUtils.verifyToken(loginDashboardUserVO.getToken(), TEST_JWT_SECRET_KEY));
        assertFalse(JwtUtils.verifyToken(loginDashboardUserVO.getToken(), "wrongKey"));

        // test loginByDatabase
        ReflectionTestUtils.setField(dashboardUserService, "ldapTemplate", null);
        given(passwordHashService.matches(TEST_PASSWORD, TEST_PASSWORD)).willReturn(true);
        assertLoginSuccessful(dashboardUserDO, dashboardUserService.login(TEST_USER_NAME, TEST_PASSWORD, null));
        verify(dashboardUserMapper, times(2)).selectByUserName(eq(TEST_USER_NAME));
        assertLoginSuccessful(dashboardUserDO, dashboardUserService.login(TEST_USER_NAME, TEST_PASSWORD, null));
        verify(dashboardUserMapper, times(3)).selectByUserName(eq(TEST_USER_NAME));

        // test loginByDatabase AES password
        SecretProperties secretPropertiesTmp = new SecretProperties();
        secretPropertiesTmp.setKey(TEST_AES_KEY);
        secretPropertiesTmp.setIv(TEST_AES_IV);
        ReflectionTestUtils.setField(dashboardUserService, "secretProperties", secretPropertiesTmp);
        ReflectionTestUtils.setField(dashboardUserService, "ldapTemplate", null);
        given(passwordHashService.matches(TEST_PASSWORD, TEST_PASSWORD)).willReturn(true);
        assertLoginSuccessful(dashboardUserDO, dashboardUserService.login(TEST_USER_NAME, AesUtils.cbcEncrypt(TEST_AES_KEY, TEST_AES_IV, TEST_PASSWORD), null));
        verify(dashboardUserMapper, times(4)).selectByUserName(eq(TEST_USER_NAME));
        assertLoginSuccessful(dashboardUserDO, dashboardUserService.login(TEST_USER_NAME, AesUtils.cbcEncrypt(TEST_AES_KEY, TEST_AES_IV, TEST_PASSWORD), null));
        verify(dashboardUserMapper, times(5)).selectByUserName(eq(TEST_USER_NAME));

        // test loginByDatabase plain password fallback when secret endpoint does not provide key material
        given(passwordHashService.matches(TEST_PASSWORD, TEST_PASSWORD)).willReturn(true);
        assertLoginSuccessful(dashboardUserDO, dashboardUserService.login(TEST_USER_NAME, TEST_PASSWORD, null));
        verify(dashboardUserMapper, times(6)).selectByUserName(eq(TEST_USER_NAME));
    }

    @Test
    public void testLoginMigratesLegacySha512Password() {
        ReflectionTestUtils.setField(dashboardUserService, "jwtProperties", jwtProperties);
        given(jwtProperties.getSecretKey()).willReturn(TEST_JWT_SECRET_KEY);
        given(jwtProperties.getExpiredSeconds()).willReturn(86400000L);
        ReflectionTestUtils.setField(dashboardUserService, "ldapTemplate", null);
        DashboardUserDO legacyUser = createDashboardUserDO();
        legacyUser.setPassword(DigestUtils.sha512Hex(TEST_PASSWORD));
        when(dashboardUserMapper.selectByUserName(eq(TEST_USER_NAME))).thenReturn(legacyUser);
        given(passwordHashService.matches(TEST_PASSWORD, legacyUser.getPassword())).willReturn(false);
        given(passwordHashService.matchesLegacySha512(TEST_PASSWORD, legacyUser.getPassword())).willReturn(true);
        given(passwordHashService.encode(TEST_PASSWORD)).willReturn("bcrypt-encoded");

        LoginDashboardUserVO loginDashboardUserVO = dashboardUserService.login(TEST_USER_NAME, TEST_PASSWORD, null);
        assertEquals("bcrypt-encoded", loginDashboardUserVO.getPassword());
        verify(dashboardUserMapper).updateSelective(argThat(user ->
                TEST_ID.equals(user.getId())
                        && "bcrypt-encoded".equals(user.getPassword())
                        && Objects.nonNull(user.getDateUpdated())));
    }

    @Test
    public void testLoginDoesNotMigrateBcryptPassword() {
        ReflectionTestUtils.setField(dashboardUserService, "jwtProperties", jwtProperties);
        given(jwtProperties.getSecretKey()).willReturn(TEST_JWT_SECRET_KEY);
        given(jwtProperties.getExpiredSeconds()).willReturn(86400000L);
        ReflectionTestUtils.setField(dashboardUserService, "ldapTemplate", null);
        DashboardUserDO bcryptUser = createDashboardUserDO();
        bcryptUser.setPassword("$2b$12$VRoSQ/.z8C/ldOO9TBfclesgVQ8BxyQK/4Rg/e.DNCisEd.gSyCBG");
        when(dashboardUserMapper.selectByUserName(eq(TEST_USER_NAME))).thenReturn(bcryptUser);
        given(passwordHashService.matches(TEST_PASSWORD, bcryptUser.getPassword())).willReturn(true);

        LoginDashboardUserVO loginDashboardUserVO = dashboardUserService.login(TEST_USER_NAME, TEST_PASSWORD, null);
        assertEquals(bcryptUser.getPassword(), loginDashboardUserVO.getPassword());
        verify(dashboardUserMapper, never()).updateSelective(any(DashboardUserDO.class));
    }

    @Test
    public void testModifyPasswordAcceptsLegacySha512OldPassword() {
        DashboardUserDO legacyUser = createDashboardUserDO();
        legacyUser.setPassword(DigestUtils.sha512Hex("oldPassword"));
        given(dashboardUserMapper.selectById(TEST_ID)).willReturn(legacyUser);
        given(dashboardUserMapper.updateSelective(any(DashboardUserDO.class))).willReturn(1);
        given(passwordHashService.matches("oldPassword", legacyUser.getPassword())).willReturn(false);
        given(passwordHashService.matchesLegacySha512("oldPassword", legacyUser.getPassword())).willReturn(true);
        given(passwordHashService.encode("newPassword")).willReturn("bcrypt-new-password");

        assertEquals(1, dashboardUserService.modifyPassword(
                new org.apache.shenyu.admin.model.dto.DashboardUserModifyPasswordDTO(TEST_ID, TEST_USER_NAME, "newPassword", "oldPassword")));
        verify(dashboardUserMapper).updateSelective(argThat(user ->
                TEST_ID.equals(user.getId()) && "bcrypt-new-password".equals(user.getPassword())));
    }

    @Test
    public void testModifyPasswordBumpsTokenVersion() {
        DashboardUserDO before = createDashboardUserDO();
        before.setTokenVersion(3L);
        given(dashboardUserMapper.selectById(TEST_ID)).willReturn(before);
        given(passwordHashService.matches("oldPassword", TEST_PASSWORD)).willReturn(true);
        given(passwordHashService.encode("newPassword")).willReturn("bcrypt-new-password");
        given(dashboardUserMapper.updateSelective(any(DashboardUserDO.class))).willReturn(1);

        assertEquals(1, dashboardUserService.modifyPassword(
                new org.apache.shenyu.admin.model.dto.DashboardUserModifyPasswordDTO(TEST_ID, TEST_USER_NAME, "newPassword", "oldPassword")));

        // the version moves in SQL, never as a value computed from a possibly stale read
        verify(dashboardUserMapper).incrementTokenVersion(eq(TEST_ID), any(Timestamp.class));
        ArgumentCaptor<DashboardUserDO> captor = ArgumentCaptor.forClass(DashboardUserDO.class);
        verify(dashboardUserMapper).updateSelective(captor.capture());
        assertNull(captor.getValue().getTokenVersion());
    }

    @Test
    public void testAdministrativePasswordResetBumpsTokenVersion() {
        SessionUtil.setLocalVisitor(UserInfo.builder().userId("1").userName("admin").build());
        DashboardUserDO before = createDashboardUserDO();
        before.setTokenVersion(2L);
        given(dashboardUserMapper.selectById(TEST_ID)).willReturn(before);
        given(passwordHashService.isBcryptHash("newPassword")).willReturn(true);
        given(dashboardUserMapper.updateSelective(any(DashboardUserDO.class))).willReturn(1);

        DashboardUserDTO dashboardUserDTO = DashboardUserDTO.builder()
                .id(TEST_ID).userName(TEST_USER_NAME).password("newPassword").enabled(true).build();
        assertEquals(1, dashboardUserService.update(dashboardUserDTO));

        verify(dashboardUserMapper).incrementTokenVersion(eq(TEST_ID), any(Timestamp.class));
    }

    @Test
    public void testRevokeTokensBumpsTokenVersion() {
        SessionUtil.setLocalVisitor(UserInfo.builder().userId("1").userName("admin").build());
        DashboardUserDO before = createDashboardUserDO();
        before.setTokenVersion(0L);
        DashboardUserDO after = createDashboardUserDO();
        after.setTokenVersion(1L);
        given(dashboardUserMapper.selectById(TEST_ID)).willReturn(before, after);
        given(dashboardUserMapper.incrementTokenVersion(eq(TEST_ID), any(Timestamp.class))).willReturn(1);

        assertEquals(1, dashboardUserService.revokeTokens(TEST_ID));

        verify(dashboardUserMapper).incrementTokenVersion(eq(TEST_ID), any(Timestamp.class));
        // no absolute version write: a delayed revoke cannot reuse a stale value
        verify(dashboardUserMapper, never()).updateSelective(any(DashboardUserDO.class));
        // the audit event carries the stored row, not a partially populated one
        verify(publisher).onUpdated(eq(after), eq(before));
    }

    @Test
    public void testRevokeTokensRequiresAdmin() {
        SessionUtil.setLocalVisitor(UserInfo.builder().userId("2").userName("editor").build());
        assertThrows(ValidFailException.class, () -> dashboardUserService.revokeTokens(TEST_ID));
    }

    @Test
    public void testLoginAuditSuccessEvent() {
        ReflectionTestUtils.setField(dashboardUserService, "ldapTemplate", null);
        given(jwtProperties.getSecretKey()).willReturn(TEST_JWT_SECRET_KEY);
        given(jwtProperties.getExpiredSeconds()).willReturn(86400000L);
        given(dashboardUserMapper.selectByUserName(eq(TEST_USER_NAME))).willReturn(createDashboardUserDO());
        given(passwordHashService.matches(TEST_PASSWORD, TEST_PASSWORD)).willReturn(true);
        List<ILoggingEvent> events = captureAuditLogs(() -> dashboardUserService.login(TEST_USER_NAME, TEST_PASSWORD, null));
        assertEquals(1, events.size());
        String message = events.get(0).getFormattedMessage();
        assertEquals(Level.INFO, events.get(0).getLevel());
        assertTrue(message.contains("eventType=login"), message);
        assertTrue(message.contains("outcome=success"), message);
        assertTrue(message.contains("reason=success"), message);
        assertTrue(message.contains("actor=unauthenticated"), message);
        assertTrue(message.contains("submittedIdentity=" + TEST_USER_NAME), message);
        assertFalse(message.contains(TEST_PASSWORD), message);
    }

    @Test
    public void testLoginAuditBadCredentialsEvent() {
        ReflectionTestUtils.setField(dashboardUserService, "ldapTemplate", null);
        given(dashboardUserMapper.selectByUserName(eq(TEST_USER_NAME))).willReturn(null);
        List<ILoggingEvent> events = captureAuditLogs(() -> dashboardUserService.login(TEST_USER_NAME, TEST_PASSWORD, null));
        assertEquals(1, events.size());
        String message = events.get(0).getFormattedMessage();
        assertEquals(Level.WARN, events.get(0).getLevel());
        assertTrue(message.contains("outcome=failure"), message);
        assertTrue(message.contains("reason=bad_credentials"), message);
        assertTrue(message.contains("actor=unauthenticated"), message);
        assertTrue(message.contains("requestId=no-request"), message);
        assertTrue(message.contains("resource=n/a"), message);
        assertFalse(message.contains(TEST_PASSWORD), message);
    }

    @Test
    public void testLoginAuditDisabledAccountEvent() {
        ReflectionTestUtils.setField(dashboardUserService, "ldapTemplate", null);
        DashboardUserDO disabledUser = createDashboardUserDO();
        disabledUser.setEnabled(false);
        given(dashboardUserMapper.selectByUserName(eq(TEST_USER_NAME))).willReturn(disabledUser);
        given(passwordHashService.matches(TEST_PASSWORD, TEST_PASSWORD)).willReturn(true);
        List<ILoggingEvent> events = captureAuditLogs(() -> dashboardUserService.login(TEST_USER_NAME, TEST_PASSWORD, null));
        assertEquals(1, events.size());
        String message = events.get(0).getFormattedMessage();
        assertEquals(Level.WARN, events.get(0).getLevel());
        assertTrue(message.contains("outcome=denied"), message);
        assertTrue(message.contains("reason=account_disabled"), message);
        assertTrue(message.contains("actor=unauthenticated"), message);
    }

    @Test
    public void testLoginAuditSanitizesSubmittedIdentity() {
        ReflectionTestUtils.setField(dashboardUserService, "ldapTemplate", null);
        given(dashboardUserMapper.selectByUserName(anyString())).willReturn(null);
        List<ILoggingEvent> events = captureAuditLogs(() -> {
            dashboardUserService.login("ad\r\nmin8179", TEST_PASSWORD, null);
            dashboardUserService.login("a".repeat(200), TEST_PASSWORD, null);
        });
        assertEquals(2, events.size());
        String first = events.get(0).getFormattedMessage();
        assertTrue(first.contains("submittedIdentity=admin8179"), first);
        assertFalse(first.contains("\r"), first);
        assertFalse(first.contains("\n"), first);
        String second = events.get(1).getFormattedMessage();
        assertTrue(second.contains("submittedIdentity=" + "a".repeat(128)), second);
        assertFalse(second.contains("a".repeat(129)), second);
    }

    @Test
    public void testLoginAuditNeverRecordsPasswordOrExceptionDetail() {
        SecretProperties realSecret = new SecretProperties();
        realSecret.setKey(TEST_AES_KEY);
        realSecret.setIv(TEST_AES_IV);
        ReflectionTestUtils.setField(dashboardUserService, "secretProperties", realSecret);
        LdapProperties ldapProperties = new LdapProperties();
        ldapProperties.setBaseDn("test");
        ReflectionTestUtils.setField(dashboardUserService, "ldapProperties", ldapProperties);
        ReflectionTestUtils.setField(dashboardUserService, "ldapTemplate", ldapTemplate);
        final String plainPassword = "shenyu8179-unique-plain-password";
        final String encryptedPassword = AesUtils.cbcEncrypt(TEST_AES_KEY, TEST_AES_IV, plainPassword);
        given(dashboardUserMapper.selectByUserName(anyString())).willReturn(null);
        given(ldapTemplate.authenticate(anyString(), anyString(), anyString()))
                .willThrow(new RuntimeException("sensitive-ldap-message-8179"));
        List<ILoggingEvent> events = captureAuditLogs(() -> dashboardUserService.login(TEST_USER_NAME, encryptedPassword, null));
        assertEquals(1, events.size());
        String message = events.get(0).getFormattedMessage();
        assertTrue(message.contains("reason=bad_credentials"), message);
        assertFalse(message.contains(plainPassword), message);
        assertFalse(message.contains(encryptedPassword), message);
        assertFalse(message.contains("sensitive-ldap-message-8179"), message);
    }

    @Test
    public void testLdapVerifyFailureLogDoesNotLeakExceptionOrSensitiveConfig() {
        LdapProperties ldapProperties = new LdapProperties();
        ldapProperties.setBaseDn("dc=shenyu,dc=org");
        ReflectionTestUtils.setField(dashboardUserService, "ldapProperties", ldapProperties);
        ReflectionTestUtils.setField(dashboardUserService, "ldapTemplate", ldapTemplate);
        final String sensitiveDetail = "ldap://ldap.internal:389/dc=shenyu,dc=org searchBase=uid=userName,dc=shenyu,dc=org";
        given(ldapTemplate.authenticate(anyString(), anyString(), anyString()))
                .willThrow(new RuntimeException(sensitiveDetail));
        given(dashboardUserMapper.selectByUserName(anyString())).willReturn(null);
        ch.qos.logback.classic.Logger serviceLogger = (ch.qos.logback.classic.Logger) LoggerFactory.getLogger(DashboardUserServiceImpl.class);
        ListAppender<ILoggingEvent> appender = new ListAppender<>();
        appender.start();
        serviceLogger.addAppender(appender);
        final LoginDashboardUserVO result;
        try {
            result = dashboardUserService.login(TEST_USER_NAME, "plain-password-8179", null);
        } finally {
            serviceLogger.detachAppender(appender);
        }
        // the ldap failure still falls back to the database lookup, and the login simply fails
        verify(dashboardUserMapper).selectByUserName(eq(TEST_USER_NAME));
        assertNull(result);
        assertEquals(1, appender.list.size());
        ILoggingEvent event = appender.list.get(0);
        assertEquals(Level.WARN, event.getLevel());
        assertEquals("ldap verify error, type=RuntimeException", event.getFormattedMessage());
        String message = event.getFormattedMessage();
        assertFalse(message.contains(sensitiveDetail), message);
        assertFalse(message.contains("ldap.internal"), message);
        assertFalse(message.contains("dc=shenyu"), message);
        assertNull(event.getThrowableProxy(), "no exception stack trace may be attached to the log event");
    }

    @Test
    public void testLoginAuditUsesRequestContextWhenPresent() {
        ReflectionTestUtils.setField(dashboardUserService, "ldapTemplate", null);
        given(dashboardUserMapper.selectByUserName(eq(TEST_USER_NAME))).willReturn(null);
        MockHttpServletRequest request = new MockHttpServletRequest();
        request.setRequestURI("/platform/login");
        request.setRemoteAddr("203.0.113.10");
        RequestContextHolder.setRequestAttributes(new ServletRequestAttributes(request));
        MDC.put(SecurityAuditLog.REQUEST_ID_MDC_KEY, "login-req-8179");
        List<ILoggingEvent> events = captureAuditLogs(() -> dashboardUserService.login(TEST_USER_NAME, TEST_PASSWORD, null));
        assertEquals(1, events.size());
        String message = events.get(0).getFormattedMessage();
        assertTrue(message.contains("resource=/platform/login"), message);
        assertTrue(message.contains("remoteAddr=203.0.113.10"), message);
        assertTrue(message.contains("requestId=login-req-8179"), message);
    }

    @Test
    public void testLoginAuditRecordsExactlyOneEventPerLoginCall() {
        ReflectionTestUtils.setField(dashboardUserService, "ldapTemplate", null);
        given(jwtProperties.getSecretKey()).willReturn(TEST_JWT_SECRET_KEY);
        given(jwtProperties.getExpiredSeconds()).willReturn(86400000L);
        given(dashboardUserMapper.selectByUserName(eq(TEST_USER_NAME))).willReturn(createDashboardUserDO());
        given(passwordHashService.matches(TEST_PASSWORD, TEST_PASSWORD)).willReturn(true);
        // both the POST and the deprecated GET login endpoints delegate to this single method,
        // so two calls must yield exactly two audit records with no same-layer duplicates.
        List<ILoggingEvent> events = captureAuditLogs(() -> {
            dashboardUserService.login(TEST_USER_NAME, TEST_PASSWORD, null);
            dashboardUserService.login(TEST_USER_NAME, TEST_PASSWORD, null);
        });
        long loginEvents = events.stream().filter(event -> event.getFormattedMessage().contains("eventType=login")).count();
        assertEquals(2, loginEvents);
    }

    /**
     * capture the logback events emitted on the security audit logger while running the given action.
     *
     * @param runnable action to run
     * @return captured logging events
     */
    private List<ILoggingEvent> captureAuditLogs(final Runnable runnable) {
        ch.qos.logback.classic.Logger logger = (ch.qos.logback.classic.Logger) LoggerFactory.getLogger(SecurityAuditLog.LOGGER_NAME);
        ListAppender<ILoggingEvent> appender = new ListAppender<>();
        appender.start();
        logger.addAppender(appender);
        try {
            runnable.run();
        } finally {
            logger.detachAppender(appender);
        }
        return appender.list;
    }

    private DashboardUserDO createDashboardUserDO() {
        return DashboardUserDO.builder()
                .id(TEST_ID).userName(TEST_USER_NAME).password(TEST_PASSWORD)
                .enabled(true)
                .dateCreated(new Timestamp(System.currentTimeMillis()))
                .dateUpdated(new Timestamp(System.currentTimeMillis()))
                .build();
    }

    private void assertLoginSuccessful(final DashboardUserDO dashboardUserDO, final DashboardUserVO dashboardUserVO) {
        assertEquals(dashboardUserDO.getId(), dashboardUserVO.getId());
        assertEquals(dashboardUserDO.getUserName(), dashboardUserVO.getUserName());
        assertEquals(dashboardUserDO.getPassword(), dashboardUserVO.getPassword());
    }
}
