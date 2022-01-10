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

package org.apache.shenyu.admin.config;

import org.apache.shenyu.admin.config.properties.LdapProperties;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.ldap.core.LdapOperations;
import org.springframework.ldap.core.LdapTemplate;
import org.springframework.ldap.core.support.LdapContextSource;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

/**
 * LdapConfiguration.
 */
@Configuration
@ConditionalOnProperty(name = "shenyu.ldap.enabled", havingValue = "true")
@EnableConfigurationProperties(LdapProperties.class)
public class LdapConfiguration {

    /**
     * register ldap client in spring ioc.
     *
     * @param ldapProp the ldap configuration
     * @return LdapContextSource {@linkplain LdapContextSource}
     */
    @Bean
    @ConditionalOnMissingBean
    public LdapContextSource contextSource(final LdapProperties ldapProp) {
        return Optional.ofNullable(ldapProp).map(item -> {
            LdapContextSource contextSource = new LdapContextSource();
            contextSource.setUrl(item.getUrl());
            contextSource.setUserDn(item.getBindDn());
            contextSource.setPassword(item.getPassword());
            contextSource.setPooled(true);
            Map<String, Object> config = new HashMap<>();
            config.put("java.naming.ldap.attributes.binary", "objectGUID");
            config.put("com.sun.jndi.ldap.connect.timeout", String.valueOf(item.getConnectTimeout()));
            config.put("com.sun.jndi.ldap.read.timeout", String.valueOf(item.getReadTimeout()));
            contextSource.setBaseEnvironmentProperties(config);
            return contextSource;
        }).orElse(null);
    }

    /**
     * register ldap template in spring ioc.
     *
     * @param ldapContextSource the ldap context source
     * @return LdapTemplate {@linkplain LdapTemplate}
     */
    @Bean
    @ConditionalOnMissingBean(LdapOperations.class)
    public LdapTemplate ldapTemplate(final LdapContextSource ldapContextSource) {
        return new LdapTemplate(ldapContextSource);
    }

}
