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

package org.dromara.soul.admin.interceptor;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.dromara.soul.admin.interceptor.annotation.DataPermission;
import org.dromara.soul.admin.model.query.RuleQuery;
import org.dromara.soul.admin.model.query.SelectorQuery;
import org.dromara.soul.admin.utils.JwtUtils;
import org.dromara.soul.common.constant.AdminConstants;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * data permission aop interceptor.
 *
 * @author nuo-promise
 */
@Aspect
@Component
@Slf4j
public class DataPermissionInterceptor {

    /**
     * define data permission aop point cut.
     */
    @Pointcut("@annotation(org.dromara.soul.admin.interceptor.annotation.DataPermission)")
    public void dataPermissionCut() { }


    /**
     * Real method processing around.
     *
     * @param point point {@link ProceedingJoinPoint}
     * @return result {@link Object}
     */
    @SneakyThrows
    @Around("dataPermissionCut()")
    public Object around(final ProceedingJoinPoint point) {
        List<String> dataPermissionList = getUserDataPermission(JwtUtils.getUserId());
        Object[] args = point.getArgs();
        if (dataPermissionList.size() > 0) {
            DataPermission dataPermission = ((MethodSignature) point.getSignature()).getMethod().getAnnotation(DataPermission.class);
            if (dataPermission != null && args != null) {
                if (dataPermission.dataType().equals(AdminConstants.DATA_PERMISSION_SELECTOR)) {
                    SelectorQuery selectorQuery = (SelectorQuery) args[0];
                    selectorQuery.setFilterIds(dataPermissionList);
                    args[0] = selectorQuery;
                } else if (dataPermission.dataType().equals(AdminConstants.DATA_PERMISSION_RULE)) {
                    RuleQuery ruleQuery = (RuleQuery) args[0];
                    ruleQuery.setFilterIds(dataPermissionList);
                    args[0] = ruleQuery;
                }
            }
        }
        return point.proceed(args);
    }

    /**
     * Determine whether the user has opened data permissions.
     *
     * @param userId user id
     * @return true or false {@link Boolean}
     */
    private List<String> getUserDataPermission(final String userId) {
//        return Arrays.asList("1371700260516032512", "1371700260641861632", "1371700280225067008");
        return new ArrayList<>();
    }

}
