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

package org.apache.shenyu.admin.mybatis.oracle;

import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;

import java.io.StringReader;
import java.sql.CallableStatement;
import java.sql.Clob;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Objects;

/**
 * Type handler for binding and reading large Oracle CLOB values as strings.
 */
public final class OracleClobStringTypeHandler extends BaseTypeHandler<String> {

    @Override
    public void setNonNullParameter(final PreparedStatement preparedStatement, final int index,
                                    final String parameter, final JdbcType jdbcType) throws SQLException {
        preparedStatement.setClob(index, new StringReader(parameter), parameter.length());
    }

    @Override
    public String getNullableResult(final ResultSet resultSet, final String columnName) throws SQLException {
        return readClob(resultSet.getClob(columnName));
    }

    @Override
    public String getNullableResult(final ResultSet resultSet, final int columnIndex) throws SQLException {
        return readClob(resultSet.getClob(columnIndex));
    }

    @Override
    public String getNullableResult(final CallableStatement callableStatement, final int columnIndex) throws SQLException {
        return readClob(callableStatement.getClob(columnIndex));
    }

    private String readClob(final Clob clob) throws SQLException {
        if (Objects.isNull(clob)) {
            return null;
        }
        try {
            long length = clob.length();
            if (length > Integer.MAX_VALUE) {
                throw new SQLException("Oracle CLOB is too large to convert to a String");
            }
            if (length == 0) {
                return "";
            }
            return clob.getSubString(1, (int) length);
        } finally {
            clob.free();
        }
    }
}
