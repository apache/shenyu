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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shiro.authc.AuthenticationInfo;
import org.apache.shiro.authc.AuthenticationToken;
import org.apache.shiro.authc.SimpleAuthenticationInfo;
import org.apache.shiro.authz.AuthorizationInfo;
import org.apache.shiro.authz.SimpleAuthorizationInfo;
import org.apache.shiro.realm.AuthorizingRealm;
import org.apache.shiro.subject.PrincipalCollection;
import org.dromara.soul.admin.shiro.bean.StatelessToken;
import org.dromara.soul.admin.utils.JwtUtils;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

/**
 * shiro custom's realm.
 *
 * @author YuI
 **/
public class ShiroRealm extends AuthorizingRealm {

    @Override
    protected AuthorizationInfo doGetAuthorizationInfo(final PrincipalCollection principalCollection) {
        // todo: temporary, will use database to add permission
        Set<String> permissions = new HashSet<>(Collections.singletonList("sys:*:*"));
        if (CollectionUtils.isEmpty(permissions)) {
            return null;
        }
        SimpleAuthorizationInfo simpleAuthorizationInfo = new SimpleAuthorizationInfo();
        simpleAuthorizationInfo.setStringPermissions(permissions);

        return simpleAuthorizationInfo;
    }

    @Override
    protected AuthenticationInfo doGetAuthenticationInfo(final AuthenticationToken authenticationToken) {
        StatelessToken token = (StatelessToken) authenticationToken;
        if (token == null || token.getToken() == null) {
            return null;
        }

        String userName = JwtUtils.getIssuer(token.getToken());

        token.setUserName(userName);

        return new SimpleAuthenticationInfo(userName, token.getToken(), this.getName());
    }
}
