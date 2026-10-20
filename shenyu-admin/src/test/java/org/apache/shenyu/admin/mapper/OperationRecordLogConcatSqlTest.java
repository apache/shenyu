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

import org.apache.ibatis.builder.xml.XMLMapperBuilder;
import org.apache.ibatis.io.Resources;
import org.apache.ibatis.mapping.BoundSql;
import org.apache.ibatis.session.Configuration;
import org.apache.shenyu.admin.model.query.RecordLogQueryCondition;
import org.junit.jupiter.api.Test;

import java.io.InputStream;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Verifies the operation-log keyword search uses Oracle-compatible CONCAT.
 */
public final class OperationRecordLogConcatSqlTest {

    @Test
    public void testKeywordSearchUsesTwoArgumentConcat() throws Exception {
        Configuration configuration = new Configuration();
        configuration.setDatabaseId("oracle");
        try (InputStream in = Resources.getResourceAsStream("mappers/operation-record-log-sqlmap.xml")) {
            new XMLMapperBuilder(in, configuration,
                    "mappers/operation-record-log-sqlmap.xml", configuration.getSqlFragments()).parse();
        }
        RecordLogQueryCondition condition = new RecordLogQueryCondition();
        condition.setKeyword("keyword");
        condition.setExcluded("excluded");
        Map<String, Object> parameters = new HashMap<>();
        parameters.put("condition", condition);
        BoundSql boundSql = configuration
                .getMappedStatement("org.apache.shenyu.admin.mapper.OperationRecordLogMapper.selectByCondition")
                .getBoundSql(parameters);
        String sql = boundSql.getSql().toLowerCase(Locale.ROOT);
        String compact = sql.replaceAll("\\s+", "");
        assertFalse(compact.contains("concat('%',?,'%')"), sql);
        assertTrue(compact.contains("concat(concat('%',?),'%')"), sql);
    }
}
