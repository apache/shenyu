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

package org.apache.shenyu.admin.controller;

import org.apache.commons.collections4.CollectionUtils;
import jakarta.annotation.Resource;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.aspect.annotation.RestApi;
import org.apache.shenyu.admin.disruptor.ShenyuHttpRequestRecordDisruptorPublisher;
import org.apache.shenyu.admin.model.dto.ShenyuHttpRequestRecordDTO;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.record.entity.ReplayProgress;
import org.apache.shenyu.admin.service.HttpRecordService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;
import java.util.zip.CRC32;

@RestApi("/record")
public class HttpRecordController {

    private static final Logger LOG = LoggerFactory.getLogger(HttpRecordController.class);

    @Resource
    private ShenyuHttpRequestRecordDisruptorPublisher publisher;

    @Resource
    private HttpRecordService httpRecordService;

    @PostMapping("/upload-record")
    public String upload(
            @RequestHeader(value = Constants.X_RECORD_CRC32) final Long clientCrc32,
            @RequestBody final byte[] recordBytes) {
        try {
            if (Objects.nonNull(clientCrc32)) {
                CRC32 crc32 = new CRC32();
                crc32.update(recordBytes);
                long serverCrc32 = crc32.getValue();
                if (serverCrc32 != clientCrc32) {
                    LOG.warn("Record CRC32 validation failed! Client: {}, Server: {}. Data dropped silently.", clientCrc32, serverCrc32);
                    return ShenyuResultMessage.SUCCESS;
                }
            }
            String recordBody = new String(recordBytes, StandardCharsets.UTF_8);
            List<ShenyuHttpRequestRecordDTO.Record> records = GsonUtils.getInstance().fromList(
                    recordBody,
                    ShenyuHttpRequestRecordDTO.Record.class
            );
            if (CollectionUtils.isNotEmpty(records)) {
                ShenyuHttpRequestRecordDTO dto = new ShenyuHttpRequestRecordDTO();
                dto.setRecords(records);
                publisher.publish(dto);
            }
            return ShenyuResultMessage.SUCCESS;
        } catch (Exception e) {
            LOG.warn("Failed to parse record body, data dropped silently. Reason: {}", e.getMessage());
            return ShenyuResultMessage.SUCCESS;
        }
    }

    /**
     * List all available record tasks that can be replayed.
     *
     * @return list of record task metadata
     */
    @GetMapping("")
    public ShenyuAdminResult listRecordTasks() {
        return ShenyuAdminResult.success(httpRecordService.listRecordTasks());
    }

    /**
     * Start an asynchronous replay of all records belonging to the given taskId.
     *
     * @param taskId     maps to {storagePath}/{taskId}.jsonl
     * @param targetHost base URL of the gateway to replay against, e.g. http://127.0.0.1:9195
     * @return replayId for polling progress
     */
    @PostMapping("/replay")
    public ShenyuAdminResult startReplay(
            @RequestParam final String taskId,
            @RequestParam final String targetHost) {
        if (StringUtils.isAnyBlank(taskId, targetHost)) {
            return ShenyuAdminResult.error("taskId and targetHost must not be blank");
        }
        try {
            String replayId = httpRecordService.startReplay(taskId, targetHost);
            return ShenyuAdminResult.success(null, replayId);
        } catch (IllegalArgumentException e) {
            return ShenyuAdminResult.error(e.getMessage());
        } catch (Exception e) {
            LOG.error("Failed to start replay. taskId={}, targetHost={}", taskId, targetHost, e);
            return ShenyuAdminResult.error("Failed to start replay: " + e.getMessage());
        }
    }

    /**
     * Poll the progress of an ongoing or completed replay job.
     *
     * @param replayId the id returned by {@link #startReplay}
     * @return current progress snapshot
     */
    @GetMapping("/progress/{replayId}")
    public ShenyuAdminResult getReplayProgress(@PathVariable final String replayId) {
        ReplayProgress progress = httpRecordService.getReplayProgress(replayId);
        if (Objects.isNull(progress)) {
            return ShenyuAdminResult.error("Replay not found for replayId: " + replayId);
        }
        return ShenyuAdminResult.success(progress);
    }
}
