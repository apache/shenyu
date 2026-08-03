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

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import okhttp3.Response;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.config.properties.HttpRecordProperties;
import org.apache.shenyu.admin.model.dto.ShenyuHttpRequestRecordDTO;
import org.apache.shenyu.admin.record.HttpRecordRepository;
import org.apache.shenyu.admin.record.entity.RecordTaskInfo;
import org.apache.shenyu.admin.record.entity.ReplayProgress;
import org.apache.shenyu.admin.service.HttpRecordService;
import org.apache.shenyu.admin.utils.HttpUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

@Service
public class HttpRecordServiceImpl implements HttpRecordService {

    private static final Logger LOG = LoggerFactory.getLogger(HttpRecordServiceImpl.class);

    private final HttpUtils httpUtils;

    private final ThreadPoolExecutor replayExecutor;

    private final HttpRecordProperties properties;

    private final Cache<String, ReplayProgress> progressMap = Caffeine.newBuilder()
            .expireAfterWrite(1, TimeUnit.HOURS)
            .build();

    private final HttpRecordRepository httpRecordRepository;

    public HttpRecordServiceImpl(final ThreadPoolExecutor replayExecutor,
                                 final HttpRecordProperties properties, final HttpUtils httpUtils, final Map<String, HttpRecordRepository> repositoryMap) {
        this.replayExecutor = replayExecutor;
        this.properties = properties;
        this.httpUtils = httpUtils;
        this.httpRecordRepository = repositoryMap.getOrDefault(properties.getStorageType(), repositoryMap.get("local"));
    }


    @Override
    public String startReplay(final String taskId, final String targetHost) {
        List<ShenyuHttpRequestRecordDTO.Record> records = httpRecordRepository.loadRecords(taskId);
        if (records.isEmpty()) {
            throw new IllegalArgumentException("No records found for taskId: " + taskId);
        }

        String replayId = UUIDUtils.getInstance().generateShortUuid();
        ReplayProgress progress = new ReplayProgress(replayId, taskId, records.size());
        progressMap.put(replayId, progress);

        for (ShenyuHttpRequestRecordDTO.Record record : records) {
            replayExecutor.submit(() -> replayRecord(record, targetHost, progress));
        }

        LOG.info("Replay started. replayId={}, taskId={}, totalRecords={}, targetHost={}",
                replayId, taskId, records.size(), targetHost);
        return replayId;
    }

    @Override
    public ReplayProgress getReplayProgress(final String replayId) {
        return progressMap.getIfPresent(replayId);
    }


    @Override
    public List<RecordTaskInfo> listRecordTasks() {
        return httpRecordRepository.listRecordTasks();
    }

    private void replayRecord(final ShenyuHttpRequestRecordDTO.Record record,
                              final String targetHost, final ReplayProgress progress) {
        String url = buildUrl(targetHost, record.getRequestUri(), record.getQueryParams());
        try {
            doReplay(url, record);
            progress.incrementSucceeded();
        } catch (Exception e) {
            LOG.warn("Failed to replay record. method={}, url={}, traceId={}, error={}",
                    record.getMethod(), url, record.getTraceId(), e.getMessage());
            progress.incrementFailed();
        }
    }

    private void doReplay(final String url, final ShenyuHttpRequestRecordDTO.Record record) throws IOException {
        Map<String, String> headers = buildReplayHeaders(record.getRequestHeaders());
        String body = StringUtils.defaultString(record.getRequestBody(), "");
        HttpUtils.HTTPMethod method = HttpUtils.HTTPMethod.fromValue(record.getMethod());
        try (Response response = httpUtils.requestJson(url, body, headers, method)) {
            LOG.debug("Replayed {} {} -> status={}, traceId={}",
                    record.getMethod(), url, response.code(), record.getTraceId());
        }
    }

    private String buildUrl(final String targetHost, final String requestUri, final String queryParams) {
        String base = StringUtils.stripEnd(targetHost, "/") + requestUri;
        if (StringUtils.isNotBlank(queryParams)) {
            return base + "?" + queryParams;
        }
        return base;
    }

    private Map<String, String> buildReplayHeaders(final Map<String, String> originalHeaders) {
        Map<String, String> headers = new HashMap<>();
        if (Objects.nonNull(originalHeaders)) {
            for (Map.Entry<String, String> entry : originalHeaders.entrySet()) {
                String name = entry.getKey();
                if ("Content-Length".equalsIgnoreCase(name) || "Host".equalsIgnoreCase(name)) {
                    continue;
                }
                headers.put(name, entry.getValue());
            }
        }
        headers.put(Constants.X_SHENYU_REPLAY, "true");
        return headers;
    }

}
