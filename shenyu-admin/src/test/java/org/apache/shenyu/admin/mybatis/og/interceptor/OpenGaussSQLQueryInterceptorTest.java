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

package org.apache.shenyu.admin.mybatis.og.interceptor;

import org.apache.ibatis.cache.CacheKey;
import org.apache.ibatis.executor.Executor;
import org.apache.ibatis.mapping.BoundSql;
import org.apache.ibatis.mapping.MappedStatement;
import org.apache.ibatis.plugin.Invocation;
import org.apache.ibatis.session.ResultHandler;
import org.apache.ibatis.session.RowBounds;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Properties;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class OpenGaussSQLQueryInterceptorTest {

    @Test
    public void interceptTest() throws SQLException {
        final OpenGaussSQLQueryInterceptor openGaussSQLQueryInterceptor = new OpenGaussSQLQueryInterceptor();
        final Invocation invocation = mock(Invocation.class);
        Object[] args = new Object[4];
        args[0] = mock(MappedStatement.class);
        args[1] = mock(Object.class);
        args[2] = mock(RowBounds.class);
        args[3] = mock(ResultHandler.class);
        when(invocation.getArgs()).thenReturn(args);
        final Executor executor = mock(Executor.class);
        when(invocation.getTarget()).thenReturn(executor);
        when(executor.createCacheKey(any(), any(), any(), any())).thenReturn(mock(CacheKey.class));
        when(executor.query(any(), any(), any(), any(), any(), any())).thenReturn(new ArrayList<>());
        Assertions.assertDoesNotThrow(() -> openGaussSQLQueryInterceptor.intercept(invocation));
        args = new Object[6];
        args[0] = mock(MappedStatement.class);
        args[1] = mock(Object.class);
        args[2] = mock(RowBounds.class);
        args[3] = mock(ResultHandler.class);
        args[4] = mock(CacheKey.class);
        args[5] = mock(BoundSql.class);
        when(invocation.getArgs()).thenReturn(args);
        Assertions.assertDoesNotThrow(() -> openGaussSQLQueryInterceptor.intercept(invocation));
    }

    @Test
    public void pluginTest() {
        final OpenGaussSQLQueryInterceptor openGaussSQLQueryInterceptor = new OpenGaussSQLQueryInterceptor();
        Assertions.assertDoesNotThrow(() -> openGaussSQLQueryInterceptor.plugin(new Object()));
    }

    @Test
    public void setPropertiesTest() {
        final OpenGaussSQLQueryInterceptor openGaussSQLQueryInterceptor = new OpenGaussSQLQueryInterceptor();
        Assertions.assertDoesNotThrow(() -> openGaussSQLQueryInterceptor.setProperties(mock(Properties.class)));
    }

}
