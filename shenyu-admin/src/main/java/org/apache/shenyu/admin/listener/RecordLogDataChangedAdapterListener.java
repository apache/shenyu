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

package org.apache.shenyu.admin.listener;

import org.apache.shenyu.admin.jpa.repository.OperationRecordLogRepository;
import org.apache.shenyu.admin.model.entity.OperationRecordLog;
import org.apache.shenyu.admin.model.event.AdminDataModelChangedEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.stereotype.Component;

/**
 * RecordLogDataChangedAdapterListener.
 */
@Component
public class RecordLogDataChangedAdapterListener implements DataChangedListener, ApplicationListener<AdminDataModelChangedEvent> {

    private final OperationRecordLogRepository operationRecordLogRepository;

    public RecordLogDataChangedAdapterListener(final OperationRecordLogRepository operationRecordLogRepository) {
        this.operationRecordLogRepository = operationRecordLogRepository;
    }

    @Override
    public void onApplicationEvent(final AdminDataModelChangedEvent event) {
        if (event.isConsumed()) {
            return;
        }
        final OperationRecordLog log = new OperationRecordLog();
        log.setColor(event.getType().getColor());
        log.setContext(event.buildContext());
        log.setOperationTime(event.getDate());
        log.setOperationType(event.getType().getTypeName());
        log.setOperator(event.getOperator());
        operationRecordLogRepository.save(log);
        event.consumed();
    }

}
