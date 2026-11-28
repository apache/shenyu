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

import org.apache.ibatis.executor.statement.StatementHandler;
import org.apache.ibatis.mapping.BoundSql;
import org.apache.ibatis.plugin.Invocation;
import org.apache.ibatis.session.Configuration;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.parallel.ResourceLock;
import org.junit.jupiter.api.parallel.Resources;

import java.util.Collections;
import java.util.Locale;
import java.util.Properties;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * OpenGaussSQLPrepareInterceptorTest.
 */
public class OpenGaussSQLPrepareInterceptorTest {

    @Test
    public void interceptTest() {
        final OpenGaussSQLPrepareInterceptor openGaussSQLPrepareInterceptor = new OpenGaussSQLPrepareInterceptor();
        final Invocation invocation = mock(Invocation.class);
        final StatementHandler statementHandler = mock(StatementHandler.class);
        when(invocation.getTarget()).thenReturn(statementHandler);
        final BoundSql boundSql = mock(BoundSql.class);
        when(statementHandler.getBoundSql()).thenReturn(boundSql);
        when(boundSql.getSql()).thenReturn("select * from users where name = 1 limit 1");
        Assertions.assertDoesNotThrow(() -> openGaussSQLPrepareInterceptor.intercept(invocation));
    }

    @Test
    @ResourceLock(Resources.LOCALE)
    public void testPrepareUsesLocaleIndependentSqlForMultilineInsert() {
        Locale original = Locale.getDefault();
        Locale display = Locale.getDefault(Locale.Category.DISPLAY);
        Locale format = Locale.getDefault(Locale.Category.FORMAT);
        try {
            Locale.setDefault(Locale.forLanguageTag("tr-TR"));
            BoundSql boundSql = new BoundSql(new Configuration(),
                    "INSERT INTO `ITEMS` (ID)\nVALUES (?), (?)", Collections.emptyList(), null);
            StatementHandler statementHandler = mock(StatementHandler.class);
            when(statementHandler.getBoundSql()).thenReturn(boundSql);
            StatementHandler wrapped = (StatementHandler) new OpenGaussSQLPrepareInterceptor().plugin(statementHandler);
            Assertions.assertDoesNotThrow(() -> wrapped.prepare(null, null));
            Assertions.assertEquals("insert into \"items\" (id)\nvalues (?), (?)", boundSql.getSql());
        } finally {
            Locale.setDefault(original);
            Locale.setDefault(Locale.Category.DISPLAY, display);
            Locale.setDefault(Locale.Category.FORMAT, format);
        }
    }

    @Test
    public void pluginTest() {
        final OpenGaussSQLPrepareInterceptor openGaussSQLPrepareInterceptor = new OpenGaussSQLPrepareInterceptor();
        Assertions.assertDoesNotThrow(() -> openGaussSQLPrepareInterceptor.plugin(new Object()));
    }

    @Test
    public void setPropertiesTest() {
        final OpenGaussSQLPrepareInterceptor openGaussSQLPrepareInterceptor = new OpenGaussSQLPrepareInterceptor();
        Assertions.assertDoesNotThrow(() -> openGaussSQLPrepareInterceptor.setProperties(mock(Properties.class)));
    }
}
