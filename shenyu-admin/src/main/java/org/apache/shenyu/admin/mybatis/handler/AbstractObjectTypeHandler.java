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

package org.apache.shenyu.admin.mybatis.handler;

import org.apache.ibatis.type.BaseTypeHandler;
import org.apache.ibatis.type.JdbcType;
import org.apache.shenyu.common.utils.GsonUtils;
import org.springframework.util.StringUtils;

import java.sql.CallableStatement;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * AbstractObjectTypeHandler.
 */
public abstract class AbstractObjectTypeHandler<T> extends BaseTypeHandler<T> {
    
    @Override
    public void setNonNullParameter(final PreparedStatement ps, final int i, final Object parameter,
                                    final JdbcType jdbcType) throws SQLException {
        ps.setString(i, GsonUtils.getGson().toJson(parameter));
    }
    
    @Override
    public T getNullableResult(final ResultSet rs, final String columnName)
            throws SQLException {
        String data = rs.getString(columnName);
        return StringUtils.hasText(data) ? GsonUtils.getGson().fromJson(data, (Class<T>) getRawType()) : null;
    }
    
    @Override
    public T getNullableResult(final ResultSet rs, final int columnIndex) throws SQLException {
        String data = rs.getString(columnIndex);
        return StringUtils.hasText(data) ? GsonUtils.getGson().fromJson(data, (Class<T>) getRawType()) : null;
    }
    
    @Override
    public T getNullableResult(final CallableStatement cs, final int columnIndex)
            throws SQLException {
        String data = cs.getString(columnIndex);
        return StringUtils.hasText(data) ? GsonUtils.getGson().fromJson(data, (Class<T>) getRawType()) : null;
    }
}
