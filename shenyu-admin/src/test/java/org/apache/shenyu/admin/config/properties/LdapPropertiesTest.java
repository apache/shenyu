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

package org.apache.shenyu.admin.config.properties;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * Test cases for {@link JwtProperties}.
 */
public class LdapPropertiesTest {
    
    @Test
    public void jwtPropertiesTest() {
        final LdapProperties ldapProperties = new LdapProperties();
        ldapProperties.setUrl("url");
        ldapProperties.setBaseDn("baseDn");
        ldapProperties.setEnabled(true);
        ldapProperties.setConnectTimeout(0);
        ldapProperties.setPassword("password");
        ldapProperties.setLoginField("loginField");
        ldapProperties.setObjectClass("objectClass");
        ldapProperties.setReadTimeout(0);
        ldapProperties.setBindDn("bindDn");
        Assertions.assertEquals(ldapProperties.getUrl(), "url");
        Assertions.assertEquals(ldapProperties.getBaseDn(), "baseDn");
        Assertions.assertTrue(ldapProperties.isEnabled());
        Assertions.assertEquals(ldapProperties.getConnectTimeout(), 0);
        Assertions.assertEquals(ldapProperties.getPassword(), "password");
        Assertions.assertEquals(ldapProperties.getLoginField(), "loginField");
        Assertions.assertEquals(ldapProperties.getObjectClass(), "objectClass");
        Assertions.assertEquals(ldapProperties.getReadTimeout(), 0);
        Assertions.assertEquals(ldapProperties.getBindDn(), "bindDn");
    }
}
