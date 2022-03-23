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

package org.apache.shenyu.admin.service.converter;

import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;

import java.util.List;

/**
 * The interface Selector handle converter.
 */
public interface SelectorHandleConverter {
    
    /**
     * Convert upstream list.
     *
     * @param handle the handle
     * @return the list
     */
    List<CommonUpstream> convertUpstream(String handle);
    
    /**
     * Plugin name string.
     *
     * @return the string
     */
    String pluginName();
    
    /**
     * Handler string.
     *
     * @param handle the handle
     * @param aliveList the upstream live list
     * @return the string
     */
    String handler(String handle, List<CommonUpstream> aliveList);

    /**
     * Update upstream status and remove invalid.
     *
     * @param <T> the type parameter
     * @param existList the existList
     * @param aliveList the aliveList
     * @return the valid existList
     */
    <T extends CommonUpstream> List<T> updateStatusAndFilter(List<T> existList, List<? extends CommonUpstream> aliveList);
}
