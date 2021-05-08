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

package org.apache.shenyu.admin.interceptor;

import lombok.SneakyThrows;
import org.apache.shenyu.admin.interceptor.annotation.DataPermission;
import org.apache.shenyu.admin.service.DataPermissionService;
import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Aspect;
import org.aspectj.lang.annotation.Pointcut;
import org.aspectj.lang.reflect.MethodSignature;
import org.apache.shenyu.admin.model.query.RuleQuery;
import org.apache.shenyu.admin.model.query.SelectorQuery;
import org.apache.shenyu.admin.utils.JwtUtils;
import org.apache.shenyu.common.constant.AdminConstants;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * data permission aop interceptor.
 *
 * @author nuo-promise
 */
@Aspect
@Component
public class DataPermissionInterceptor {

    private final DataPermissionService dataPermissionService;

    public DataPermissionInterceptor(final DataPermissionService dataPermissionService) {
        this.dataPermissionService = dataPermissionService;
    }

    /**
     * define data permission aop point cut.
     */
    @Pointcut("@annotation(org.apache.shenyu.admin.interceptor.annotation.DataPermission)")
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
        return point.proceed(getFilterSQLData(point));
    }

    /**
     * Organize SQL parameters with data permissions.
     *
     * @param point {@link ProceedingJoinPoint}
     * @return args {@link List}
     */
    private Object[] getFilterSQLData(final ProceedingJoinPoint point) {
        Object[] args = point.getArgs();
        List<String> dataPermissionList = dataPermissionService.getDataPermission(JwtUtils.getUserId());
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
        return args;
    }
}
