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
import org.apache.ibatis.session.Configuration;
import org.apache.shenyu.admin.model.query.DetailQuery;
import org.junit.jupiter.api.Test;

import java.io.InputStream;
import java.util.Locale;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Verifies the Oracle variant of the detail mapper statements.
 */
public final class DetailMapperOracleSqlTest {

    private static final String MAPPER = "mappers/detail-sqlmap.xml";

    private static final String SELECT_BY_QUERY = "org.apache.shenyu.admin.mapper.DetailMapper.selectByQuery";

    @Test
    public void testSelectByQueryComparesClobOnOracle() throws Exception {
        String sql = selectByQuerySql("oracle", new DetailQuery("value", "desc", null));
        assertTrue(sql.contains("dbms_lob.compare(field_value, ?) = 0"), sql);
        assertFalse(sql.contains("field_value = ?"), sql);
        assertTrue(sql.contains("value_desc = ?"), sql);
    }

    @Test
    public void testSelectByQueryWithoutConditionsOnOracle() throws Exception {
        String sql = selectByQuerySql("oracle", new DetailQuery(null, null, null));
        assertFalse(sql.contains("where"), sql);
    }

    @Test
    public void testSelectByQueryKeepsEqualityOnOtherDatabases() throws Exception {
        String sql = selectByQuerySql(null, new DetailQuery("value", null, null));
        assertTrue(sql.contains("field_value = ?"), sql);
        assertFalse(sql.contains("dbms_lob"), sql);
    }

    private String selectByQuerySql(final String databaseId, final DetailQuery query) throws Exception {
        Configuration configuration = new Configuration();
        configuration.setDatabaseId(databaseId);
        try (InputStream in = Resources.getResourceAsStream(MAPPER)) {
            new XMLMapperBuilder(in, configuration, MAPPER, configuration.getSqlFragments()).parse();
        }
        return configuration.getMappedStatement(SELECT_BY_QUERY).getBoundSql(query).getSql()
                .toLowerCase(Locale.ROOT).replaceAll("\\s+", " ");
    }
}
