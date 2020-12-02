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

package org.dromara.soul.spi;

import org.dromara.soul.spi.fixture.JdbcSPI;
import org.dromara.soul.spi.fixture.MysqlSPI;
import org.dromara.soul.spi.fixture.NopSPI;
import org.hamcrest.CoreMatchers;
import org.junit.Test;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.fail;

public final class ExtensionLoaderTest {

    @Test
    public void testSPI() {
        JdbcSPI jdbcSPI = ExtensionLoader.getExtensionLoader(JdbcSPI.class).getJoin("mysql");
        assertThat(jdbcSPI.getClass().getName(), is(MysqlSPI.class.getName()));
    }

    @Test
    public void test_getExtensionLoader_NotInterface() {
        try {
            ExtensionLoader.getExtensionLoader(ExtensionLoaderTest.class);
            fail();
        } catch (IllegalArgumentException expected) {
            assertThat(expected.getMessage(),
                    CoreMatchers.containsString("extension clazz (class org.dromara.soul.spi.ExtensionLoaderTest) is not interface!"));
        }
    }

    @Test
    public void test_getExtensionLoader_NotSpiAnnotation() {
        try {
            ExtensionLoader.getExtensionLoader(NopSPI.class);
            fail();
        } catch (IllegalArgumentException expected) {
            assertThat(expected.getMessage(),
                    CoreMatchers.containsString("extension clazz (interface org.dromara.soul.spi.fixture.NopSPI) without @interface org.dromara.soul.spi.SPI Annotation"));
        }
    }
}
