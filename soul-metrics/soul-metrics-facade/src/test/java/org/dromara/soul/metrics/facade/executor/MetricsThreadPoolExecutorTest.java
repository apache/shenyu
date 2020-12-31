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

package org.dromara.soul.metrics.facade.executor;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import java.util.Optional;
import java.util.concurrent.atomic.AtomicReference;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * The Test Case For MetricsThreadPollExecutor.
 *
 * @author nuo-promise
 */
public class MetricsThreadPoolExecutorTest {

    private MetricsThreadPoolExecutor metricsThreadPoolExecutor;

    @Before
    public void setUp() {
        metricsThreadPoolExecutor = new MetricsThreadPoolExecutor(1, 1);
    }

    @After
    public void tearDown() {
        if (Optional.ofNullable(metricsThreadPoolExecutor).isPresent()) {
            metricsThreadPoolExecutor.shutdown();
        }
    }

    @Test
    public void testMetricsThreadPoolExecutor() {
        AtomicReference<Integer> count = new AtomicReference<>(0);
        for (int i = 1; i <= 5; i++) {
            int finalI = i;
            metricsThreadPoolExecutor.execute(() -> {
                count.getAndSet(count.get() + 1);
                assertThat(count.get(), is(finalI));
            });
        }
    }
}
