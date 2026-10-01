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

import org.apache.ibatis.executor.Executor;
import org.apache.ibatis.mapping.MappedStatement;
import org.apache.ibatis.plugin.Invocation;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.sql.SQLException;
import java.util.Properties;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class OracleSQLUpdateInterceptorTest {

    @Test
    public void interceptNullParameterTest() throws SQLException {
        final OracleSQLUpdateInterceptor interceptor = new OracleSQLUpdateInterceptor();
        final Invocation invocation = mock(Invocation.class);
        Object[] args = new Object[2];
        args[0] = mock(MappedStatement.class);
        final Executor executor = mock(Executor.class);
        when(invocation.getTarget()).thenReturn(executor);
        when(invocation.getArgs()).thenReturn(args);
        when(executor.update(any(), isNull())).thenReturn(1);
        Assertions.assertDoesNotThrow(() -> interceptor.intercept(invocation));
        verify(executor).update(any(), isNull());
    }

    @Test
    public void pluginTest() {
        final OracleSQLUpdateInterceptor interceptor = new OracleSQLUpdateInterceptor();
        Assertions.assertDoesNotThrow(() -> interceptor.plugin(new Object()));
    }

    @Test
    public void setPropertiesTest() {
        final OracleSQLUpdateInterceptor interceptor = new OracleSQLUpdateInterceptor();
        Assertions.assertDoesNotThrow(() -> interceptor.setProperties(mock(Properties.class)));
    }
}
