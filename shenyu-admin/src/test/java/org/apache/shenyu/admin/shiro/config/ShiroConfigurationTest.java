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

import org.apache.shenyu.admin.config.properties.ShiroProperties;
import org.apache.shiro.realm.AuthorizingRealm;
import org.apache.shiro.spring.LifecycleBeanPostProcessor;
import org.apache.shiro.spring.security.interceptor.AuthorizationAttributeSourceAdvisor;
import org.apache.shiro.spring.web.ShiroFilterFactoryBean;
import org.apache.shiro.web.mgt.DefaultWebSecurityManager;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.aop.framework.autoproxy.DefaultAdvisorAutoProxyCreator;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link ShiroConfiguration}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ShiroConfigurationTest {
    
    @InjectMocks
    private ShiroConfiguration shiroConfiguration;
    
    @Mock
    private DefaultWebSecurityManager securityManager;
    
    @Test
    public void testSecurityManager() {
        AuthorizingRealm realm = mock(AuthorizingRealm.class);
        DefaultWebSecurityManager securityManager = shiroConfiguration.securityManager(realm);
        Object[] realms = securityManager.getRealms().toArray();
        assertEquals(1, realms.length);
        assert Objects.equals(realm, realms[0]);
    }
    
    @Test
    public void testShiroFilterFactoryBean() {
        ShiroProperties shiroProperties = mock(ShiroProperties.class);
        List<String> whiteList = Arrays.asList("test1", "test2");
        when(shiroProperties.getWhiteList()).thenReturn(whiteList);
        ShiroFilterFactoryBean shiroFilterFactoryBean = shiroConfiguration.shiroFilterFactoryBean(securityManager, shiroProperties);
        assertEquals(securityManager, shiroFilterFactoryBean.getSecurityManager());
        assertNotNull(shiroFilterFactoryBean.getFilters());
        assertNotNull(shiroFilterFactoryBean.getFilters().get("statelessAuth"));
        Map<String, String> map = shiroFilterFactoryBean.getFilterChainDefinitionMap();
        assertNotNull(map);
        whiteList.stream().forEach(s -> assertEquals("anon", map.get(s)));
        assertEquals("statelessAuth", map.get("/**"));
    }
    
    @Test
    public void testAuthorizationAttributeSourceAdvisor() {
        AuthorizationAttributeSourceAdvisor advisor = shiroConfiguration.authorizationAttributeSourceAdvisor(securityManager);
        assertEquals(securityManager, advisor.getSecurityManager());
    }
    
    @Test
    public void testGetDefaultAdvisorAutoProxyCreator() {
        DefaultAdvisorAutoProxyCreator creator = shiroConfiguration.getDefaultAdvisorAutoProxyCreator();
        assertNotNull(creator);
        assertEquals(true, creator.isProxyTargetClass());
    }
    
    @Test
    public void testLifecycleBeanPostProcessor() {
        LifecycleBeanPostProcessor postProcessor = shiroConfiguration.lifecycleBeanPostProcessor();
        assertNotNull(postProcessor);
    }
}
