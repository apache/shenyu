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

package org.apache.shenyu.admin.mybatis.og.handler;

import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;
import org.apache.ibatis.type.MappedTypes;
import org.apache.shenyu.admin.mybatis.og.enums.NullEnum;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * OpenGaussSQL's custom type handler for Boolean types.
 */
@MappedTypes(value = String.class)
public class OpenGaussSQLStringHandler extends BaseTypeHandler<String> {

    @Override
    public void setNonNullParameter(final PreparedStatement preparedStatement, final int columnIndex, final String s, final JdbcType jdbcType) throws SQLException {
        preparedStatement.setString(columnIndex, convertEnum(s));
    }

    @Override
    public String getNullableResult(final ResultSet resultSet, final String s) throws SQLException {
        return convert(resultSet.getString(s));
    }

    @Override
    public String getNullableResult(final ResultSet resultSet, final int i) throws SQLException {
        return convert(resultSet.getString(i));
    }

    @Override
    public String getNullableResult(final CallableStatement callableStatement, final int i) throws SQLException {
        return convert(callableStatement.getString(i));
    }

    private String convert(final String s) {
        if (NullEnum.NULL.getLabel().equals(s)) {
            return NullEnum.NULL.getValue();
        }
        return s;
    }

    private String convertEnum(final String s) {
        if (NullEnum.NULL.getValue().equals(s)) {
            return NullEnum.NULL.getLabel();
        }
        return s;
    }

}
