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

import java.util.LinkedList;
import java.util.Map;

/**
 * The interface M bean receiver.
 */
public interface MBeanReceiver {
    
    /**
     * Record bean.
     *
     * @param domain          the domain
     * @param beanProperties  the bean properties
     * @param attrKeys        the attr keys
     * @param attrName        the attr name
     * @param attrType        the attr type
     * @param attrDescription the attr description
     * @param value           the value
     */
    void recordBean(String domain, Map<String, String> beanProperties,
                    LinkedList<String> attrKeys, String attrName, String attrType,
                    String attrDescription, Object value);
}
