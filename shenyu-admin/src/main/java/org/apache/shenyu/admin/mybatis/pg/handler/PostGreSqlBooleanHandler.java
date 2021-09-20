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

import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;
import org.apache.ibatis.type.MappedTypes;
import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * PostgreSql's custom type handler for Boolean types.
 */
@MappedTypes(value = Boolean.class)
public class PostGreSqlBooleanHandler extends BaseTypeHandler<Boolean> {

    @Override
    public void setNonNullParameter(final PreparedStatement preparedStatement, final int i,
                                    final Boolean aBoolean, final JdbcType jdbcType) throws SQLException {
        preparedStatement.setInt(i, aBoolean ? 1 : 0);
    }

    @Override
    public Boolean getNullableResult(final ResultSet resultSet, final String s) throws SQLException {
        int anInt = resultSet.getInt(s);
        return 1 == anInt ? Boolean.TRUE : Boolean.FALSE;
    }

    @Override
    public Boolean getNullableResult(final ResultSet resultSet, final int i) throws SQLException {
        int anInt = resultSet.getInt(i);
        return 1 == anInt ? Boolean.TRUE : Boolean.FALSE;
    }

    @Override
    public Boolean getNullableResult(final CallableStatement callableStatement, final int i) throws SQLException {
        int anInt = callableStatement.getInt(i);
        return 1 == anInt ? Boolean.TRUE : Boolean.FALSE;
    }
}
