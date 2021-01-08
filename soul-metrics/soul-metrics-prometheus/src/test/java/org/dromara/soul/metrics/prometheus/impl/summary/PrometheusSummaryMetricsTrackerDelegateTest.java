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

package org.dromara.soul.metrics.prometheus.impl.summary;

import io.prometheus.client.Summary;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * Test cases for PrometheusSummaryMetricsTrackerDelegate.
 *
 * @author nuo-promise
 * @author dengliming
 */
@RunWith(MockitoJUnitRunner.class)
public final class PrometheusSummaryMetricsTrackerDelegateTest {

    @Mock
    private Summary.Timer timer;

    @Test
    public void observeDuration() {
        PrometheusSummaryMetricsTrackerDelegate prometheusSummaryMetricsTrackerDelegate = new PrometheusSummaryMetricsTrackerDelegate(timer);
        prometheusSummaryMetricsTrackerDelegate.observeDuration();
        assertThat(timer.observeDuration(), is(0.0d));
    }
}
