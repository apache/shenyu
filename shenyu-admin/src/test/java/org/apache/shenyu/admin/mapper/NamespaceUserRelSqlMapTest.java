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

package org.apache.shenyu.admin.mapper;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for namespace-user-rel-sqlmap.
 */
class NamespaceUserRelSqlMapTest {

    private static final String MAPPER_RESOURCE = "mappers/namespace-user-rel-sqlmap.xml";

    @Test
    void batchSaveShouldUseForeachItemForAllBindings() throws IOException {
        String mapper = readMapper();

        assertTrue(mapper.contains("<foreach collection=\"namespaceUserRelDOList\" item=\"namespaceUserRelDO\""));
        List.of("id", "namespaceId", "userId", "dateCreated", "dateUpdated").forEach(property ->
                assertTrue(mapper.contains("#{namespaceUserRelDO." + property + ",")));
        assertFalse(mapper.contains("#{namespacePluginRelDO."));
    }

    private String readMapper() throws IOException {
        try (InputStream inputStream = getClass().getClassLoader().getResourceAsStream(MAPPER_RESOURCE)) {
            if (Objects.isNull(inputStream)) {
                throw new IOException("Mapper resource not found: " + MAPPER_RESOURCE);
            }
            return new String(inputStream.readAllBytes(), StandardCharsets.UTF_8);
        }
    }
}
