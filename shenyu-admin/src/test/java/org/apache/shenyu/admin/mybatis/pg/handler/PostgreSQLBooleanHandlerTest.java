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

package org.apache.shenyu.admin.mybatis.pg.handler;

import org.apache.ibatis.type.JdbcType;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;

import static org.mockito.Mockito.mock;

/**
 * PostgreSQLBooleanHandlerTest.
 */
public class PostgreSQLBooleanHandlerTest {

    @Test
    public void setNonNullParameterTest() {
        final PostgreSQLBooleanHandler postgreSQLBooleanHandler = new PostgreSQLBooleanHandler();
        Assertions.assertDoesNotThrow(() -> postgreSQLBooleanHandler.setNonNullParameter(mock(PreparedStatement.class), 1, true, JdbcType.BIGINT));
    }

    @Test
    public void getNullableResultTest() {
        final PostgreSQLBooleanHandler postgreSQLBooleanHandler = new PostgreSQLBooleanHandler();
        final ResultSet resultSet = mock(ResultSet.class);
        Assertions.assertDoesNotThrow(() -> postgreSQLBooleanHandler.getNullableResult(resultSet, 1));
        Assertions.assertDoesNotThrow(() -> postgreSQLBooleanHandler.getNullableResult(resultSet, "column"));
        final CallableStatement callableStatement = mock(CallableStatement.class);
        Assertions.assertDoesNotThrow(() -> postgreSQLBooleanHandler.getNullableResult(callableStatement, 1));
    }
}
