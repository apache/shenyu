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

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * admin dashboard properties.
 */
@Configuration
@ConfigurationProperties(prefix = "shenyu.dashboard.core")
public class DashboardProperties {
    
    /**
     * record log limit.
     */
    @Value("${shenyu.dashboard.core.record-log-limit:12}")
    private Integer recordLogLimit;
    
    
    /**
     * Only supports deleting logs older than a certain day.
     * default is 3.
     */
    @Value("${shenyu.dashboard.core.record-log-only-clean-days:3}")
    private Integer onlyCleanDays;
    
    
    /**
     * enable print api log..
     * default is false.
     */
    @Value("${shenyu.dashboard.core.enable-print-api-log:false}")
    private Boolean enablePrintApiLog;
    
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
}
