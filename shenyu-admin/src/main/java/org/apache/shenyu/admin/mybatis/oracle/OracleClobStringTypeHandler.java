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
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * Binds strings as JDBC CLOBs so Oracle SQL expressions receive LOBs rather than CHAR parameters.
 */
public final class OracleClobStringTypeHandler extends BaseTypeHandler<String> {

    @Override
    public void setNonNullParameter(final PreparedStatement statement, final int index, final String value,
                                     final JdbcType jdbcType) throws SQLException {
        statement.setClob(index, new StringReader(value));
    }

    @Override
    public String getNullableResult(final ResultSet resultSet, final String column) throws SQLException {
        return resultSet.getString(column);
    }

    @Override
    public String getNullableResult(final ResultSet resultSet, final int column) throws SQLException {
        return resultSet.getString(column);
    }

    @Override
    public String getNullableResult(final CallableStatement statement, final int column) throws SQLException {
        return statement.getString(column);
    }
}
