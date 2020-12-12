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

package org.dromara.soul.common.concurrent;

import lombok.extern.slf4j.Slf4j;
import org.junit.Test;
import java.util.concurrent.ThreadFactory;
import static org.junit.Assert.assertNotNull;

/**
 * Test cases for SoulThreadFactory.
 *
 * @author FocusZhouGD
 */
@Slf4j
public final class SoulThreadFactoryTest {

    private static final String NAME_PREFIX = "soul##thread##";

    @Test
    public void testCreate() {
        ThreadFactory threadFactory = SoulThreadFactory.create(NAME_PREFIX, true);
        assertNotNull(threadFactory);
    }

    @Test
    public void testCustomCreate() {
        ThreadFactory threadFactory = SoulThreadFactory.create(NAME_PREFIX, true, 2);
        Thread thread = threadFactory.newThread(() -> Thread.currentThread().getId());
        assertNotNull(thread);

    }

    @Test
    public void testNewThread() {
        ThreadFactory threadFactory = SoulThreadFactory.create(NAME_PREFIX, true);
        threadFactory.newThread(() -> Thread.currentThread().setName("NAME_PREFIX"));
    }
}
