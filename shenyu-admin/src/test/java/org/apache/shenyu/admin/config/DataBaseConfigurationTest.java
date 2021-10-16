/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
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

import org.apache.shenyu.admin.config.properties.DataBaseProperties;
import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * The TestCase for DataBaseConfiguration.
 */
public final class DataBaseConfigurationTest {

    @Test
    public void testDataBaseProperties() {
        DataBaseConfiguration dataBaseConfiguration = new DataBaseConfiguration();
        String dialect = "MySQL";
        String initScript = "/tmp/init.sql";
        DataBaseProperties dataBaseProperties = dataBaseConfiguration.dataBaseProperties(dialect, initScript, true);
        assertNotNull(dataBaseProperties);
        assertEquals(dataBaseProperties.getDialect(), dialect);
        assertEquals(dataBaseProperties.getInitScript(), initScript);
        assertEquals(dataBaseProperties.getInitEnable(), true);
    }
}
