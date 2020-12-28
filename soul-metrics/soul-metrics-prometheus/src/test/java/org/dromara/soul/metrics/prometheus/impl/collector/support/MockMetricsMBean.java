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
public interface MockMetricsMBean {
    /**
     * get int value.
     *
     * @return the int value
     */
    Integer getIntValue();
    
    /**
     * get String value.
     *
     * @return the String value
     */
    String getStringValue();
    
    /**
     * get Date value.
     *
     * @return the date value
     */
    Date getDateValue();
    
    /**
     * get CompositeData value.
     *
     * @return the CompositeData value
     */
    CompositeData getCompositeData();
    
    /**
     * get TabularData value.
     *
     * @return the TabularData value
     */
    TabularData getTabularData();
    
    /**
     * get int array value.
     *
     * @return the int array value
     */
    int[] getIntArray();
    
    /**
     * get String array value.
     *
     * @return the String array value
     */
    String[] getStringArray();
}
