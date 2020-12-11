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

package org.dromara.soul.common.utils;

import org.hamcrest.CoreMatchers;
import org.junit.Test;

import java.util.ServiceLoader;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

/**
 * Test Cases for SpiLoadFactory.
 *
 * @author dengliming
 */
public final class SpiLoadFactoryTest {

    @Test
    public void testLoadNoImplementation() {
        try {
            SpiLoadFactory.loadFirst(SpiLoadFactoryTest.class);
            fail();
        } catch (IllegalStateException expected) {
            assertThat(expected.getMessage(),
                    CoreMatchers.containsString("No implementation defined in /META-INF/services/org.dromara.soul.common.utils.SpiLoadFactoryTest, "
                             + "please check whether the file exists and has the right implementation class!"));
        }
    }

    @Test
    public void testLoadImplementation() {
        assertEquals("org.dromara.soul.common.utils.SpiLoadFactoryTest.TestSPI", SpiLoadFactory.loadFirst(SPI.class).getClass().getCanonicalName());
    }

    @Test
    public void testLoadAll() {
        ServiceLoader<SPI> serviceLoader = SpiLoadFactory.loadAll(SPI.class);
        assertTrue(serviceLoader.iterator().hasNext());
    }

    public static class TestSPI implements SPI {

    }
}
