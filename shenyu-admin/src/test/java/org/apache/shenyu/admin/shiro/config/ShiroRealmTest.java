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

package org.apache.shenyu.admin.shiro.config;

import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.admin.model.vo.DashboardUserVO;
import org.apache.shenyu.admin.service.DashboardUserService;
import org.apache.shenyu.admin.service.PermissionService;
import org.apache.shenyu.admin.shiro.bean.StatelessToken;
import org.apache.shiro.authc.AuthenticationException;
import org.apache.shiro.authc.AuthenticationInfo;
import org.apache.shiro.authc.AuthenticationToken;
import org.apache.shiro.authz.AuthorizationInfo;
import org.apache.shiro.subject.PrincipalCollection;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
    
import java.util.HashSet;
import java.util.Set;
    
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link ShiroRealm}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ShiroRealmTest {
    
    private static final String PASSWORD = "123456";
    
    @InjectMocks
    private ShiroRealm shiroRealm;
    
    @Mock
    private PermissionService permissionService;
    
    @Mock
    private DashboardUserService dashboardUserService;

    @Test
    public void testSupports() {
        StatelessToken token = mock(StatelessToken.class);
        assertEquals(true, shiroRealm.supports(token));
    }

    @Test
    public void testDoGetAuthorizationInfo() {
        PrincipalCollection principalCollection = mock(PrincipalCollection.class);
        UserInfo userInfo = new UserInfo();

        Set<String> strings = new HashSet<>();
        when(principalCollection.getPrimaryPrincipal()).thenReturn(userInfo);
        when(permissionService.getAuthPermByUserName(any())).thenReturn(strings);
        assertNull(shiroRealm.doGetAuthorizationInfo(principalCollection));

        strings.add("test");
        when(principalCollection.getPrimaryPrincipal()).thenReturn(userInfo);
        when(permissionService.getAuthPermByUserName(any())).thenReturn(strings);
        AuthorizationInfo info = shiroRealm.doGetAuthorizationInfo(principalCollection);
        assertEquals(strings, info.getStringPermissions());
    }

    @Test
    public void testDoGetAuthenticationInfo() {
        final AuthenticationToken token = mock(AuthenticationToken.class);
        final DashboardUserVO dashboardUserVO = mock(DashboardUserVO.class);

        when(token.getCredentials()).thenReturn(null);
        assertNull(shiroRealm.doGetAuthenticationInfo(token));

        AuthenticationException exception1 = assertThrows(AuthenticationException.class, () -> {
            when(token.getCredentials()).thenReturn("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"
                    + ".eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ"
                    + ".cThIIoDvwdueQB468K5xDc5633seEFoqwxjF_xSJyQQ");
            shiroRealm.doGetAuthenticationInfo(token);
        });
        assertNotNull(exception1);

        AuthenticationException exception2 = assertThrows(AuthenticationException.class, () -> {
            when(dashboardUserService.findByUserName(any())).thenReturn(null);
            when(token.getCredentials()).thenReturn("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"
                    + ".eyJzdWIiOiIxMjM0NTY3ODkwIiwidXNlck5hbWUiOiJKb2huIERvZSIsImlhdCI6MTUxNjIzOTAyMn0"
                    + ".vZiLpzbncmNC5KL1idgfapCOTsRC0h_5XnqxaGfcLlM");
            shiroRealm.doGetAuthenticationInfo(token);
        });
        assertNotNull(exception2);

        AuthenticationException exception3 = assertThrows(AuthenticationException.class, () -> {
            when(dashboardUserService.findByUserName(any())).thenReturn(dashboardUserVO);
            when(dashboardUserVO.getPassword()).thenReturn(PASSWORD);
            when(token.getCredentials()).thenReturn("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"
                    + ".eyJzdWIiOiIxMjM0NTY3ODkwIiwidXNlck5hbWUiOiJKb2huIERvZSIsImlhdCI6MTUxNjIzOTAyMn0"
                    + ".vZiLpzbncmNC5KL1idgfapCOTsRC0h_5XnqxaGfcLlM");
            shiroRealm.doGetAuthenticationInfo(token);
        });
        assertNotNull(exception3);

        when(dashboardUserService.findByUserName(any())).thenReturn(dashboardUserVO);
        when(dashboardUserVO.getPassword()).thenReturn(PASSWORD);
        when(token.getCredentials()).thenReturn("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"
                + ".eyJzdWIiOiIxMjM0NTY3ODkwIiwidXNlck5hbWUiOiJKb2huIERvZSIsImlhdCI6MTUxNjIzOTAyMn0"
                + ".Qlpf6FdKAffgceukbi2BQYdPVf71d4Nwy0YQlkiTQFc");
        AuthenticationInfo info = shiroRealm.doGetAuthenticationInfo(token);
        assertNotNull(info);
    }
}
