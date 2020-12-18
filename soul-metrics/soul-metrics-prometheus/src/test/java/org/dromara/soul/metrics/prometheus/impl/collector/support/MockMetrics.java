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

package org.dromara.soul.metrics.prometheus.impl.collector.support;

import javax.management.openmbean.CompositeData;
import javax.management.openmbean.TabularData;
import java.util.Date;

/**
 * Mock Metrics MBean.
 *
 * @author David Liu
 */
public class MockMetrics implements MockMetricsMBean {
    
    private final Integer intValue;
    
    private final String stringValue;
    
    private final Date dateValue;
    
    private final CompositeData compositeData;
    
    private final TabularData tabularData;
    
    private final int[] intArray;
    
    private final String[] stringArray;
    
    public MockMetrics(final Integer intValue, final String stringValue, final Date dateValue, final CompositeData compositeData, final TabularData tabularData,
                       final int[] intArray, final String[] stringArray) {
        this.intValue = intValue;
        this.stringValue = stringValue;
        this.dateValue = dateValue;
        this.compositeData = compositeData;
        this.tabularData = tabularData;
        this.intArray = intArray;
        this.stringArray = stringArray;
    }
    
    @Override
    public Integer getIntValue() {
        return intValue;
    }
    
    @Override
    public String getStringValue() {
        return stringValue;
    }
    
    @Override
    public Date getDateValue() {
        return dateValue;
    }
    
    @Override
    public CompositeData getCompositeData() {
        return compositeData;
    }
    
    @Override
    public TabularData getTabularData() {
        return tabularData;
    }
    
    @Override
    public int[] getIntArray() {
        return intArray;
    }
    
    @Override
    public String[] getStringArray() {
        return stringArray;
    }
}
