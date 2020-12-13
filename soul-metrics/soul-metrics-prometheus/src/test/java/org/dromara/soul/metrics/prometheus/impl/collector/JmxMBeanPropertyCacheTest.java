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

import lombok.SneakyThrows;
import org.junit.Before;
import org.junit.Test;

import javax.management.ObjectName;
import java.util.HashSet;
import java.util.Set;

import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/**
 * The Test Case For JmxMBeanProperty.
 *
 * @author nuo-promise
 **/
public final class JmxMBeanPropertyCacheTest {

    private JmxMBeanPropertyCache jmxMBeanPropertyCache;

    @Before
    public void setUp() {
        jmxMBeanPropertyCache = new JmxMBeanPropertyCache();
    }

    @SneakyThrows
    @Test
    public void testJmxMBeanPropertyCache() {
        ObjectName objectName1 = new ObjectName("[org.apache.cassandra.metrics:type=ColumnFamily,path=home]");
        assertThat(this.jmxMBeanPropertyCache.getKeyPropertyList(objectName1).get("type"), is("ColumnFamily"));
        Set<ObjectName> objectNameSet = new HashSet<>();
        ObjectName objectName2 = new ObjectName("[org.damara.sou;.metrics:type=ColumnFamily,path=home]");
        objectNameSet.add(objectName2);
        this.jmxMBeanPropertyCache.onlyKeepMBeans(objectNameSet);
        assertThat(this.jmxMBeanPropertyCache.getKeyPropertyList(objectName1).get("type"), is("ColumnFamily"));
    }
}
