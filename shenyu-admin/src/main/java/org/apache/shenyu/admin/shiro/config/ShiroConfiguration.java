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
import org.apache.shenyu.admin.shiro.bean.StatelessAuthFilter;
import org.apache.shiro.realm.AuthorizingRealm;
import org.apache.shiro.spring.LifecycleBeanPostProcessor;
import org.apache.shiro.spring.security.interceptor.AuthorizationAttributeSourceAdvisor;
import org.apache.shiro.spring.web.ShiroFilterFactoryBean;
import org.apache.shiro.web.mgt.DefaultWebSecurityManager;
import org.springframework.aop.framework.autoproxy.DefaultAdvisorAutoProxyCreator;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;

import javax.servlet.Filter;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * shiro configuration.
 */
@Configuration
public class ShiroConfiguration {

    /**
     * generate WebSecurityManager.
     *
     * @param shiroRealm shiro's realm
     * @return {@linkplain DefaultWebSecurityManager}
     */
    @Bean("shiroSecurityManager")
    public DefaultWebSecurityManager securityManager(@Lazy @Qualifier("shiroRealm") final AuthorizingRealm shiroRealm) {
        DefaultWebSecurityManager securityManager = new DefaultWebSecurityManager();
        securityManager.setRealm(shiroRealm);
        return securityManager;
    }

    /**
     * ShiroFilterFactoryBean.
     *
     * @param securityManager {@linkplain DefaultWebSecurityManager}
     * @param shiroProperties {@linkplain ShiroProperties}
     * @return {@linkplain ShiroFilterFactoryBean}
     */
    @Bean
    public ShiroFilterFactoryBean shiroFilterFactoryBean(
            @Qualifier("shiroSecurityManager") final DefaultWebSecurityManager securityManager,
            @Qualifier("shiroProperties") final ShiroProperties shiroProperties) {
        ShiroFilterFactoryBean factoryBean = new ShiroFilterFactoryBean();
        factoryBean.setSecurityManager(securityManager);
        Map<String, Filter> filterMap = new LinkedHashMap<>();
        filterMap.put("statelessAuth", new StatelessAuthFilter());
        factoryBean.setFilters(filterMap);

        Map<String, String> filterChainDefinitionMap = new LinkedHashMap<>();

        for (String s : shiroProperties.getWhiteList()) {
            filterChainDefinitionMap.put(s, "anon");
        }

        filterChainDefinitionMap.put("/**", "statelessAuth");
        factoryBean.setFilterChainDefinitionMap(filterChainDefinitionMap);
        return factoryBean;
    }

    /**
     * AuthorizationAttributeSourceAdvisor.
     *
     * @param securityManager {@linkplain DefaultWebSecurityManager}
     * @return {@linkplain AuthorizationAttributeSourceAdvisor}
     */
    @Bean
    public AuthorizationAttributeSourceAdvisor authorizationAttributeSourceAdvisor(
            @Qualifier("shiroSecurityManager") final DefaultWebSecurityManager securityManager) {
        AuthorizationAttributeSourceAdvisor authorizationAttributeSourceAdvisor = new AuthorizationAttributeSourceAdvisor();
        authorizationAttributeSourceAdvisor.setSecurityManager(securityManager);
        return authorizationAttributeSourceAdvisor;
    }

    /**
     * Support shiro annotation.
     *
     * @return DefaultAdvisorAutoProxyCreator.
     */
    @Bean
    public static DefaultAdvisorAutoProxyCreator getDefaultAdvisorAutoProxyCreator() {
        DefaultAdvisorAutoProxyCreator defaultAdvisorAutoProxyCreator = new DefaultAdvisorAutoProxyCreator();
        defaultAdvisorAutoProxyCreator.setProxyTargetClass(true);
        return defaultAdvisorAutoProxyCreator;
    }

    /**
     * shiro's lifecycle in spring.
     *
     * @return {@linkplain LifecycleBeanPostProcessor}
     */
    @Bean
    public LifecycleBeanPostProcessor lifecycleBeanPostProcessor() {
        return new LifecycleBeanPostProcessor();
    }

}
