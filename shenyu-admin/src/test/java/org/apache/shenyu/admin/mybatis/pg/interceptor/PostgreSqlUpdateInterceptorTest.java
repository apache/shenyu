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

package org.apache.shenyu.admin.mybatis.pg.interceptor;

import org.apache.ibatis.executor.Executor;
import org.apache.ibatis.mapping.MappedStatement;
import org.apache.ibatis.plugin.Invocation;
import org.apache.shenyu.common.dto.RuleData;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.sql.SQLException;
import java.util.Properties;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * PostgreSqlUpdateInterceptorTest.
 */
public class PostgreSqlUpdateInterceptorTest {

    @Test
    public void interceptTest() throws SQLException {
        final PostgreSqlUpdateInterceptor postgreSqlUpdateInterceptor = new PostgreSqlUpdateInterceptor();
        final Invocation invocation = mock(Invocation.class);
        Object[] args = new Object[2];
        args[0] = mock(MappedStatement.class);
        args[1] = mock(RuleData.class);
        final Executor executor = mock(Executor.class);
        when(invocation.getTarget()).thenReturn(executor);
        when(invocation.getArgs()).thenReturn(args);
        when(executor.update(any(), any())).thenReturn(1);
        Assertions.assertDoesNotThrow(() -> postgreSqlUpdateInterceptor.intercept(invocation));
    }

    @Test
    public void pluginTest() {
        final PostgreSqlUpdateInterceptor postgreSqlUpdateInterceptor = new PostgreSqlUpdateInterceptor();
        Assertions.assertDoesNotThrow(() -> postgreSqlUpdateInterceptor.plugin(new Object()));
    }

    @Test
    public void setPropertiesTest() {
        final PostgreSqlUpdateInterceptor postgreSqlUpdateInterceptor = new PostgreSqlUpdateInterceptor();
        Assertions.assertDoesNotThrow(() -> postgreSqlUpdateInterceptor.setProperties(mock(Properties.class)));
    }

}
