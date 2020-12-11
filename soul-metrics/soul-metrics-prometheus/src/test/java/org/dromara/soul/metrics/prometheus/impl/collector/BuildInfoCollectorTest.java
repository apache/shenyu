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

package org.dromara.soul.metrics.prometheus.impl.collector;

import io.prometheus.client.Collector.MetricFamilySamples;
import io.prometheus.client.GaugeMetricFamily;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * The Test Case For BuildInfoCollector.
 *
 * @author nuo-promise
 **/
public final class BuildInfoCollectorTest {

    @Test
    public void collect() {
        List<MetricFamilySamples> mfs = new ArrayList<>();
        GaugeMetricFamily artifactInfo = new GaugeMetricFamily(
                "jmx_exporter_build_info",
                "A metric with a constant '1' value labeled with the version of the JMX exporter.",
                Arrays.asList("version", "name"));
        Package pkg = this.getClass().getPackage();
        String version = pkg.getImplementationVersion();
        String name = pkg.getImplementationTitle();
        artifactInfo.addMetric(Arrays.asList(version != null ? version : "unknown", name != null ? name : "unknown"), 1L);
        mfs.add(artifactInfo);
        assertThat(mfs.toString(), is("[Name: jmx_exporter_build_info Type: GAUGE Help: "
                + "A metric with a constant '1' value labeled with the version of the JMX exporter. Samples: "
                + "[Name: jmx_exporter_build_info LabelNames: [version, name] labelValues: [unknown, unknown] Value: 1.0 TimestampMs: null]]"));
    }
}
