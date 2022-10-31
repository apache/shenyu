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

package org.apache.shenyu.admin.config;

import org.apache.shenyu.admin.mybatis.pg.interceptor.PostgreSQLQueryInterceptor;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test cases for {@link MapperConfig}.
 */
@ExtendWith(MockitoExtension.class)
public class MapperConfigTest {

    @InjectMocks
    private MapperConfig.OracleSQLConfig oracleSQLConfig;

    @InjectMocks
    private MapperConfig.PostgreSQLConfig postgreSQLConfig;
    
    @Test
    public void testPostgreSQLQueryInterceptor() {
        PostgreSQLQueryInterceptor postgreSQLQueryInterceptor = postgreSQLConfig.postgreSqlQueryInterceptor();
        assertNotNull(postgreSQLQueryInterceptor);
    }

    @Test
    public void postgreSqlPrepareInterceptorTest() {
        assertNotNull(postgreSQLConfig.postgreSqlPrepareInterceptor());
    }

    @Test
    public void oracleSqlPrepareInterceptorTest() {
        assertNotNull(oracleSQLConfig.oracleSqlPrepareInterceptor());
    }

    @Test
    public void oracleSqlUpdateInterceptorTest() {
        assertNotNull(oracleSQLConfig.oracleSqlUpdateInterceptor());
    }

    @Test
    public void postgreSqlUpdateInterceptorTest() {
        assertNotNull(postgreSQLConfig.postgreSqlUpdateInterceptor());
    }
}
