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

package org.apache.shenyu.common.dto.convert.rule;

import java.util.concurrent.TimeUnit;

public class HttpRecordHandle {

    private Long startTime;

    private Long endTime;

    private String taskId;

    public static HttpRecordHandle newDefaultInstance() {
        HttpRecordHandle httpRecordHandle = new HttpRecordHandle();
        httpRecordHandle.startTime = System.currentTimeMillis();
        httpRecordHandle.endTime = System.currentTimeMillis() + TimeUnit.MINUTES.toMillis(1);
        return httpRecordHandle;
    }

    public void setTaskId(final String taskId) {
        this.taskId = taskId;
    }

    public String getTaskId() {
        return taskId;
    }

    public void setStartTime(final Long startTime) {
        this.startTime = startTime;
    }

    public Long getStartTime() {
        return startTime;
    }

    public void setEndTime(final Long endTime) {
        this.endTime = endTime;
    }

    public Long getEndTime() {
        return endTime;
    }

}
