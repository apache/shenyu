/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
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
import org.junit.Test;
import org.springframework.ldap.core.support.LdapContextSource;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * The TestCase for LdapConfiguration.
 */
public final class LdapConfigurationTest {

    @Test
    public void testContextSource() {
        LdapProperties ldapProp = mock(LdapProperties.class);
        String ldapUrl = "ldap://192.168.0.10:389";
        String user = "test";
        String pass = "123";
        when(ldapProp.getUrl()).thenReturn(ldapUrl);
        when(ldapProp.getBindDn()).thenReturn(user);
        when(ldapProp.getPassword()).thenReturn(pass);
        when(ldapProp.getConnectTimeout()).thenReturn(5000);
        when(ldapProp.getReadTimeout()).thenReturn(10000);
        LdapConfiguration ldapConfiguration = new LdapConfiguration();
        LdapContextSource ldapContextSource = ldapConfiguration.contextSource(ldapProp);
        assertNotNull(ldapContextSource);
        assertThat(ldapContextSource.getUrls().length, is(1));
        assertEquals(ldapContextSource.getUrls()[0], ldapUrl);
        assertEquals(ldapContextSource.getUserDn(), user);
        assertEquals(ldapContextSource.getPassword(), pass);
    }
}
