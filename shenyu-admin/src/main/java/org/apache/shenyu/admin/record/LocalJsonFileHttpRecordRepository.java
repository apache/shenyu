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

package org.apache.shenyu.admin.record;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.config.properties.HttpRecordProperties;
import org.apache.shenyu.admin.model.dto.ShenyuHttpRequestRecordDTO;
import org.apache.shenyu.admin.record.entity.RecordTaskInfo;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Component("local")
public class LocalJsonFileHttpRecordRepository implements HttpRecordRepository {

    private static final Logger LOG = LoggerFactory.getLogger(LocalJsonFileHttpRecordRepository.class);

    private final HttpRecordProperties properties;

    public LocalJsonFileHttpRecordRepository(final HttpRecordProperties properties) {
        this.properties = properties;
    }

    @Override
    public void save(final ShenyuHttpRequestRecordDTO dto) {

        String storagePath = properties.getStoragePath();

        List<ShenyuHttpRequestRecordDTO.Record> records = dto.getRecords();
        Map<String, List<ShenyuHttpRequestRecordDTO.Record>> groupedMap = records.stream()
                .collect(Collectors.groupingBy(ShenyuHttpRequestRecordDTO.Record::getTaskId));
        groupedMap.forEach((taskId, recordList) -> {
            try {
                StringBuilder chunkBuffer = new StringBuilder(1024 * 100);
                for (ShenyuHttpRequestRecordDTO.Record record : recordList) {
                    String jsonLine = GsonUtils.getInstance().toJson(record);
                    chunkBuffer.append(jsonLine).append(System.lineSeparator());
                }
                Path dirPath = Paths.get(storagePath);
                String fileName = String.format("%s.jsonl", taskId);
                Path targetFile = dirPath.resolve(fileName);
                if (!Files.exists(targetFile)) {
                    Files.createDirectories(targetFile.getParent());
                }
                Files.writeString(targetFile, chunkBuffer.toString(),
                        StandardOpenOption.CREATE,
                        StandardOpenOption.APPEND);
            } catch (Exception e) {
                LOG.error("storage httpRecord fail, storagePath:{}", storagePath, e);
            }
        });
    }

    @Override
    public List<RecordTaskInfo> listRecordTasks() {
        Path storageDir = Paths.get(properties.getStoragePath());
        LOG.info("Listing record tasks from storage directory: {}", storageDir);
        List<RecordTaskInfo> result = new ArrayList<>();
        if (!Files.exists(storageDir) || !Files.isDirectory(storageDir)) {
            return result;
        }
        try (Stream<Path> paths = Files.list(storageDir)) {
            paths.filter(p -> p.getFileName().toString().endsWith(".jsonl"))
                    .forEach(p -> {
                        String fileName = p.getFileName().toString();
                        String taskId = fileName.substring(0, fileName.length() - ".jsonl".length());
                        long recordCount = countLines(p);
                        try {
                            long lastModified = Files.getLastModifiedTime(p).toMillis();
                            result.add(new RecordTaskInfo(taskId, recordCount, lastModified));
                        } catch (IOException e) {
                            LOG.warn("Failed to read record file metadata: {}", p, e);
                        }
                    });
        } catch (IOException e) {
            LOG.error("Failed to list record files in: {}", storageDir, e);
        }
        return result;
    }

    @Override
    public List<ShenyuHttpRequestRecordDTO.Record> loadRecords(final String taskId) {
        Path filePath = Paths.get(properties.getStoragePath(), taskId + ".jsonl");
        List<ShenyuHttpRequestRecordDTO.Record> records = new ArrayList<>();
        if (!Files.exists(filePath)) {
            LOG.warn("Record file not found: {}", filePath);
            return records;
        }
        try (Stream<String> lines = Files.lines(filePath)) {
            lines.filter(StringUtils::isNotBlank)
                    .map(line -> GsonUtils.getInstance().fromJson(line, ShenyuHttpRequestRecordDTO.Record.class))
                    .filter(Objects::nonNull)
                    .forEach(records::add);
        } catch (IOException e) {
            LOG.error("Failed to read record file: {}", filePath, e);
        }
        return records;
    }

    private long countLines(final Path filePath) {
        try (Stream<String> lines = Files.lines(filePath)) {
            return lines.filter(StringUtils::isNotBlank).count();
        } catch (IOException e) {
            LOG.warn("Failed to count lines in: {}", filePath, e);
            return 0;
        }
    }

}
