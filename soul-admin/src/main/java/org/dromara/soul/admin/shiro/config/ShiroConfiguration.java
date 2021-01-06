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

package org.dromara.soul.admin.shiro.config;

import org.apache.shiro.realm.AuthorizingRealm;
import org.apache.shiro.spring.LifecycleBeanPostProcessor;
import org.apache.shiro.spring.security.interceptor.AuthorizationAttributeSourceAdvisor;
import org.apache.shiro.spring.web.ShiroFilterFactoryBean;
import org.apache.shiro.web.mgt.DefaultWebSecurityManager;
import org.dromara.soul.admin.shiro.bean.StatelessAuthFilter;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import javax.servlet.Filter;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * shiro configuration.
 *
 * @author YuI
 **/
@Configuration
public class ShiroConfiguration {

    /**
     * white list.
     */
    private static final List<String> WHITE_LIST;

    static {
        WHITE_LIST = new LinkedList<>();
        WHITE_LIST.add("/index");
        WHITE_LIST.add("/platform/login");
    }

    /**
     * generate AuthorizingRealm bean.
     *
     * @return {@link AuthorizingRealm}
     */
    @Bean("shiroRealm")
    public AuthorizingRealm shiroRealm() {
        return new ShiroRealm();
    }

    /**
     * generate WebSecurityManager.
     *
     * @param shiroRealm shiro's realm
     * @return {@link DefaultWebSecurityManager}
     */
    @Bean("shiroSecurityManager")
    public DefaultWebSecurityManager securityManager(@Qualifier("shiroRealm") final AuthorizingRealm shiroRealm) {
        DefaultWebSecurityManager securityManager = new DefaultWebSecurityManager();
        securityManager.setRealm(shiroRealm);
        return securityManager;
    }

    /**
     * ShiroFilterFactoryBean.
     *
     * @param securityManager {@link DefaultWebSecurityManager}
     * @return {@link ShiroFilterFactoryBean}
     */
    @Bean
    public ShiroFilterFactoryBean shiroFilterFactoryBean(
            @Qualifier("shiroSecurityManager") final DefaultWebSecurityManager securityManager) {
        ShiroFilterFactoryBean factoryBean = new ShiroFilterFactoryBean();
        factoryBean.setSecurityManager(securityManager);
        Map<String, Filter> filterMap = new LinkedHashMap<>();
        filterMap.put("statelessAuth", new StatelessAuthFilter());
        factoryBean.setFilters(filterMap);

        Map<String, String> filterChainDefinitionMap = new LinkedHashMap<>();

        for (String s : WHITE_LIST) {
            filterChainDefinitionMap.put(s, "anon");
        }

        filterChainDefinitionMap.put("/**", "statelessAuth");
        factoryBean.setFilterChainDefinitionMap(filterChainDefinitionMap);
        return factoryBean;
    }

    /**
     * AuthorizationAttributeSourceAdvisor.
     *
     * @param securityManager {@link DefaultWebSecurityManager}
     * @return {@link AuthorizationAttributeSourceAdvisor}
     */
    @Bean
    public AuthorizationAttributeSourceAdvisor authorizationAttributeSourceAdvisor(
            @Qualifier("shiroSecurityManager") final DefaultWebSecurityManager securityManager) {
        AuthorizationAttributeSourceAdvisor authorizationAttributeSourceAdvisor = new AuthorizationAttributeSourceAdvisor();
        authorizationAttributeSourceAdvisor.setSecurityManager(securityManager);
        return authorizationAttributeSourceAdvisor;
    }

    /**
     * shiro's lifecycle in spring.
     *
     * @return {@link LifecycleBeanPostProcessor}
     */
    @Bean
    public LifecycleBeanPostProcessor lifecycleBeanPostProcessor() {
        return new LifecycleBeanPostProcessor();
    }

}
