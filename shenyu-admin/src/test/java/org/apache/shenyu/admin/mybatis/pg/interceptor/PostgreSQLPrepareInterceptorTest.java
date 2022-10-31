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

import org.apache.ibatis.executor.statement.StatementHandler;
import org.apache.ibatis.mapping.BoundSql;
import org.apache.ibatis.plugin.Invocation;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.Properties;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * PostgreSQLPrepareInterceptorTest.
 */
public class PostgreSQLPrepareInterceptorTest {

    @Test
    public void interceptTest() {
        final PostgreSQLPrepareInterceptor postgreSQLPrepareInterceptor = new PostgreSQLPrepareInterceptor();
        final Invocation invocation = mock(Invocation.class);
        final StatementHandler statementHandler = mock(StatementHandler.class);
        when(invocation.getTarget()).thenReturn(statementHandler);
        final BoundSql boundSql = mock(BoundSql.class);
        when(statementHandler.getBoundSql()).thenReturn(boundSql);
        when(boundSql.getSql()).thenReturn("select * from users where name = 1 limit 1");
        Assertions.assertDoesNotThrow(() -> postgreSQLPrepareInterceptor.intercept(invocation));
    }

    @Test
    public void pluginTest() {
        final PostgreSQLPrepareInterceptor postgreSQLPrepareInterceptor = new PostgreSQLPrepareInterceptor();
        Assertions.assertDoesNotThrow(() -> postgreSQLPrepareInterceptor.plugin(new Object()));
    }

    @Test
    public void setPropertiesTest() {
        final PostgreSQLPrepareInterceptor postgreSQLPrepareInterceptor = new PostgreSQLPrepareInterceptor();
        Assertions.assertDoesNotThrow(() -> postgreSQLPrepareInterceptor.setProperties(mock(Properties.class)));
    }

}
