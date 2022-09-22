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

import org.apache.commons.collections4.CollectionUtils;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.ArrayList;
import java.util.List;

/**
 * admin dashboard properties.
 */
@Configuration
@ConfigurationProperties(prefix = "shenyu.dashboard.core")
public class DashboardProperties implements InitializingBean {
    
    /**
     * record log limit.
     */
    private Integer recordLogLimit = 12;
    
    
    /**
     * Only supports deleting logs older than a certain day.
     * default is 3.
     */
    private Integer onlyCleanDays = 3;
    
    
    /**
     * enable print api log..
     * default is false.
     */
    private Boolean enablePrintApiLog = false;
    
    /**
     * enable OnlySuperAdminPermission.
     * default is true
     */
    private Boolean enableOnlySuperAdminPermission = true;
    
    
    /**
     * Only the super administrator root user has the privileges.
     * default is 3.
     *
     * @see #afterPropertiesSet()
     */
    private List<String> onlySuperAdminPermission;
    
    /**
     * get recordLogLimit.
     *
     * @return limit
     */
    public Integer getRecordLogLimit() {
        return recordLogLimit;
    }
    
    /**
     * set recordLogLimit.
     *
     * @param recordLogLimit limit
     */
    public void setRecordLogLimit(final Integer recordLogLimit) {
        this.recordLogLimit = recordLogLimit;
    }
    
    /**
     * get onlyCleanDays.
     *
     * @return days
     */
    public Integer getOnlyCleanDays() {
        return onlyCleanDays;
    }
    
    /**
     * set onlyCleanDays.
     *
     * @param onlyCleanDays days
     */
    public void setOnlyCleanDays(final Integer onlyCleanDays) {
        this.onlyCleanDays = onlyCleanDays;
    }
    
    /**
     * get enablePrintApiLog.
     *
     * @return enablePrintApiLog
     */
    public Boolean getEnablePrintApiLog() {
        return enablePrintApiLog;
    }
    
    /**
     * set enablePrintApiLog.
     *
     * @param enablePrintApiLog enablePrintApiLog
     */
    public void setEnablePrintApiLog(final Boolean enablePrintApiLog) {
        this.enablePrintApiLog = enablePrintApiLog;
    }
    
    /**
     * get enableOnlySuperAdminPermission.
     *
     * @return enable
     */
    public Boolean getEnableOnlySuperAdminPermission() {
        return enableOnlySuperAdminPermission;
    }
    
    /**
     * set enableOnlySuperAdminPermission.
     *
     * @param enableOnlySuperAdminPermission enable
     */
    public void setEnableOnlySuperAdminPermission(final Boolean enableOnlySuperAdminPermission) {
        this.enableOnlySuperAdminPermission = enableOnlySuperAdminPermission;
    }
    
    /**
     * get onlySuperAdminPermission.
     *
     * @return super admin permission
     */
    public List<String> getOnlySuperAdminPermission() {
        return onlySuperAdminPermission;
    }
    
    /**
     * set onlySuperAdminPermission.
     *
     * @param onlySuperAdminPermission onlySuperAdminPermission
     */
    public void setOnlySuperAdminPermission(final List<String> onlySuperAdminPermission) {
        this.onlySuperAdminPermission = onlySuperAdminPermission;
    }
    
    @Override
    public void afterPropertiesSet() {
        if (!Boolean.TRUE.equals(enableOnlySuperAdminPermission)) {
            onlySuperAdminPermission = new ArrayList<>();
            return;
        }
        if (CollectionUtils.isEmpty(onlySuperAdminPermission)) {
            onlySuperAdminPermission = new ArrayList<>();
            // user
            onlySuperAdminPermission.add("system:manager:add");
            onlySuperAdminPermission.add("system:manager:edit");
            onlySuperAdminPermission.add("system:manager:delete");
            // role
            onlySuperAdminPermission.add("system:role:edit");
            onlySuperAdminPermission.add("system:role:add");
            onlySuperAdminPermission.add("system:role:delete");
            // resource
            onlySuperAdminPermission.add("system:resource:addButton");
            onlySuperAdminPermission.add("system:resource:addMenu");
            onlySuperAdminPermission.add("system:resource:editButton");
            onlySuperAdminPermission.add("system:resource:editMenu");
            onlySuperAdminPermission.add("system:resource:deleteMenu");
            onlySuperAdminPermission.add("system:resource:deleteButton");
        }
    }
}
