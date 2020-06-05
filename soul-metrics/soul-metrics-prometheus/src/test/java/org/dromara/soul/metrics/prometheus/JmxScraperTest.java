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

package org.dromara.soul.metrics.prometheus;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import javax.management.ObjectName;
import org.dromara.soul.metrics.prometheus.impl.collector.JmxMBeanPropertyCache;
import org.dromara.soul.metrics.prometheus.impl.collector.JmxScraper;
import org.dromara.soul.metrics.prometheus.impl.collector.MBeanReceiver;
import org.junit.Test;

/**
 * The type Jmx scraper test.
 */
public class JmxScraperTest {
    
    /**
     * Test jmx scraper test.
     *
     * @throws Exception the exception
     */
    @Test
    public void testJmxScraperTest() throws Exception {
        List<ObjectName> objectNames = new LinkedList<>();
        new JmxScraper("", "", "", false, objectNames, new LinkedList<>(),
                new StdoutWriter(), new JmxMBeanPropertyCache()).doScrape();
    }
    
    private static class StdoutWriter implements MBeanReceiver {
        
        public void recordBean(final String domain, final Map<String, String> beanProperties,
                               final LinkedList<String> attrKeys, final String attrName,
                               final String attrType, final String attrDescription, final Object value) {
            System.out.println(domain + attrKeys + attrName + ": " + value);
        }
    }
}
