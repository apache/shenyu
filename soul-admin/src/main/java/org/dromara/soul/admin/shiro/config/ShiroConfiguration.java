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

import org.apache.commons.lang3.StringUtils;
import org.apache.shiro.realm.AuthorizingRealm;
import org.apache.shiro.spring.LifecycleBeanPostProcessor;
import org.apache.shiro.spring.security.interceptor.AuthorizationAttributeSourceAdvisor;
import org.apache.shiro.spring.web.ShiroFilterFactoryBean;
import org.apache.shiro.web.mgt.DefaultWebSecurityManager;
import org.dromara.soul.admin.config.properties.ShiroProperties;
import org.dromara.soul.admin.shiro.bean.StatelessAuthFilter;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Lazy;

import javax.servlet.Filter;
import java.util.LinkedHashMap;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * shiro configuration.
 *
 * @author YuI
 */
@Configuration
public class ShiroConfiguration {

    /**
     * generate white list.
     *
     * @param shiroProperties shiro's properties
     * @return white list
     */
    @Bean("whiteList")
    List<String> generateWhiteList(@Qualifier("shiroProperties") final ShiroProperties shiroProperties) {
        if (StringUtils.isBlank(shiroProperties.getWhiteList())) {
            return Collections.emptyList();
        }
        return Stream
                .of(StringUtils.split(shiroProperties.getWhiteList(), ","))
                .collect(Collectors.toList());
    }

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
     * @param whiteList white list
     * @return {@linkplain ShiroFilterFactoryBean}
     */
    @Bean
    public ShiroFilterFactoryBean shiroFilterFactoryBean(
            @Qualifier("shiroSecurityManager") final DefaultWebSecurityManager securityManager,
            @Qualifier("whiteList") final List<String> whiteList) {
        ShiroFilterFactoryBean factoryBean = new ShiroFilterFactoryBean();
        factoryBean.setSecurityManager(securityManager);
        Map<String, Filter> filterMap = new LinkedHashMap<>();
        filterMap.put("statelessAuth", new StatelessAuthFilter());
        factoryBean.setFilters(filterMap);

        Map<String, String> filterChainDefinitionMap = new LinkedHashMap<>();

        for (String s : whiteList) {
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
     * shiro's lifecycle in spring.
     *
     * @return {@linkplain LifecycleBeanPostProcessor}
     */
    @Bean
    public LifecycleBeanPostProcessor lifecycleBeanPostProcessor() {
        return new LifecycleBeanPostProcessor();
    }

}
