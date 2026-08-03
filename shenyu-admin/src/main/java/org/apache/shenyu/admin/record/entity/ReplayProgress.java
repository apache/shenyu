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

package org.apache.shenyu.admin.record.entity;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.concurrent.atomic.AtomicInteger;

public class ReplayProgress {

    private final String replayId;

    private final String taskId;

    private final int total;

    private final AtomicInteger succeeded = new AtomicInteger(0);

    private final AtomicInteger failed = new AtomicInteger(0);

    public ReplayProgress(final String replayId, final String taskId, final int total) {
        this.replayId = replayId;
        this.taskId = taskId;
        this.total = total;
    }

    public void incrementSucceeded() {
        succeeded.incrementAndGet();
    }

    public void incrementFailed() {
        failed.incrementAndGet();
    }

    public String getReplayId() {
        return replayId;
    }

    public String getTaskId() {
        return taskId;
    }

    public int getTotal() {
        return total;
    }

    public int getSucceeded() {
        return succeeded.get();
    }

    public int getFailed() {
        return failed.get();
    }

    public int getCompleted() {
        return succeeded.get() + failed.get();
    }

    @JsonProperty("isFinished")
    public boolean isFinished() {
        return getCompleted() >= total;
    }
}
