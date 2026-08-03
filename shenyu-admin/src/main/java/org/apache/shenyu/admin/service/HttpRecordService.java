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

package org.apache.shenyu.admin.service;


import org.apache.shenyu.admin.record.entity.RecordTaskInfo;
import org.apache.shenyu.admin.record.entity.ReplayProgress;

import java.util.List;

public interface HttpRecordService {

    /**
     * List all available record task IDs from the storage directory.
     *
     * @return list of record task metadata
     */
    List<RecordTaskInfo> listRecordTasks();

    /**
     * Start an asynchronous replay of all records belonging to the given taskId.
     *
     * @param taskId     the task id, maps to {storagePath}/{taskId}.jsonl
     * @param targetHost base URL of the gateway to replay against
     * @return a replayId that can be used to poll progress
     */
    String startReplay(String taskId, String targetHost);

    /**
     * Return the current progress of a replay job.
     *
     * @param replayId the id returned by {@link #startReplay}
     * @return progress snapshot, or {@code null} if the id is unknown
     */
    ReplayProgress getReplayProgress(String replayId);
}
