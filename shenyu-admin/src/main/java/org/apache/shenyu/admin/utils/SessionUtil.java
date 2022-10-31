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

package org.apache.shenyu.admin.utils;

import org.apache.shenyu.admin.model.custom.UserInfo;
import org.apache.shenyu.common.constant.AdminConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

/**
 * the session is a request content.
 * Suitable for retrieving user context information from a variety of sources。
 */
public final class SessionUtil {
    
    private static final Logger LOG = LoggerFactory.getLogger(SessionUtil.class);
    
    private static final ThreadLocal<UserInfo> LOCAL_VISITOR = new InheritableThreadLocal<>();
    
    private SessionUtil() {
    }
    
    
    /**
     * visitor is login user[admin or other] / app /bootstrap.
     *
     * @return default is unknown
     */
    public static String visitorName() {
        return visitor().getUserName();
    }
    
    /**
     * visitor is login user[admin or other] / app /bootstrap.
     *
     * @return default is unknown
     */
    public static UserInfo visitor() {
        try {
            final UserInfo userInfo = LOCAL_VISITOR.get();
            if (Objects.isNull(userInfo)) {
                // try get from auth
                setLocalVisitorFromAuth();
            }
            return LOCAL_VISITOR.get();
        } catch (Exception e) {
            LOG.warn("get user info error ,not found, used default user ,it unknown");
        }
        return defaultUser();
    }
    
    /**
     * set visitor user.
     *
     * @param userInfo user info
     */
    public static void setLocalVisitor(final UserInfo userInfo) {
        LOCAL_VISITOR.set(userInfo);
    }
    
    /**
     * set visitor user.
     */
    public static void setLocalVisitorFromAuth() {
        // featureToDo:Adapting app access
        final UserInfo userInfo = JwtUtils.getUserInfo();
        LOCAL_VISITOR.set(Objects.isNull(userInfo) ? defaultUser() : userInfo);
    }
    
    /**
     * clean current session.
     */
    public static void clean() {
        LOCAL_VISITOR.remove();
    }
    
    /**
     * current user is admin.
     *
     * @return boolean
     */
    public static boolean isAdmin() {
        return AdminConstants.ADMIN_NAME.equals(visitorName());
    }
    
    private static UserInfo defaultUser() {
        return UserInfo.builder().userId("-1").userName("unknown").build();
    }
}

