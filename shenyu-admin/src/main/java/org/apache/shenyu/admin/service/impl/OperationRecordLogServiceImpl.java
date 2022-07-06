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

package org.apache.shenyu.admin.service.impl;

import org.apache.shenyu.admin.config.properties.DashboardProperties;
import org.apache.shenyu.admin.mapper.OperationRecordLogMapper;
import org.apache.shenyu.admin.model.entity.OperationRecordLog;
import org.apache.shenyu.admin.model.query.RecordLogQueryCondition;
import org.apache.shenyu.admin.service.OperationRecordLogService;
import org.apache.shenyu.admin.utils.Assert;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.common.constant.AdminConstants;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

/**
 * OperationRecordLogServiceImpl.
 */
@Service
public class OperationRecordLogServiceImpl implements OperationRecordLogService {
    
    private final OperationRecordLogMapper recordLogMapper;
    
    private final DashboardProperties dashboardProperties;
    
    public OperationRecordLogServiceImpl(final OperationRecordLogMapper recordLogMapper,
                                         final DashboardProperties dashboardProperties) {
        this.recordLogMapper = recordLogMapper;
        this.dashboardProperties = dashboardProperties;
    }
    
    @Override
    public void doConditionPreProcessing(final RecordLogQueryCondition condition) {
        condition.init();
        Assert.isTrue(condition.getEndTime().getTime() > condition.getStartTime().getTime(), "end time must be greater than start time");
        if (!AdminConstants.ADMIN_NAME.equals(SessionUtil.visitorName())) {
            condition.setUsername(SessionUtil.visitorName());
        }
    }
    
    @Override
    public List<OperationRecordLog> searchByCondition(final RecordLogQueryCondition condition) {
        return recordLogMapper.selectByCondition(condition);
    }
    
    @Override
    public List<OperationRecordLog> list() { // 用户隔离User isolation
        if (AdminConstants.ADMIN_NAME.equals(SessionUtil.visitorName())) {
            return recordLogMapper.selectLimit(null, dashboardProperties.getRecordLogLimit());
        }
        return recordLogMapper.selectLimit(SessionUtil.visitorName(), dashboardProperties.getRecordLogLimit());
    }
    
    @Override
    public boolean cleanHistory(final Date date) {
        final long supportMaxTime = System.currentTimeMillis() - (dashboardProperties.getOnlyCleanDays() * 1000 * 60 * 60 * 24);
        Assert.isTrue(date.getTime() < supportMaxTime, String.format("Only supports cleaning data older than %d days", dashboardProperties.getOnlyCleanDays()));
        return recordLogMapper.deleteByBefore(date) > 0;
    }
}
