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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.utils.GsonUtils;

import java.util.List;

/**
 * The type Abstract selector handle converter.
 */
public abstract class AbstractSelectorHandleConverter implements SelectorHandleConverter {
    
    /**
     * Do handle object.
     *
     * @param handle the handle
     * @param aliveList the alive list
     * @return the object
     */
    protected abstract Object doHandle(String handle, List<CommonUpstream> aliveList);
    
    /**
     * Handler string.
     *
     * @param handle the handle
     * @param aliveList the upstream live list
     * @return the string
     */
    @Override
    public String handler(final String handle, final List<CommonUpstream> aliveList) {
        if (StringUtils.isEmpty(handle) || CollectionUtils.isEmpty(aliveList)) {
            return "";
        }
        return GsonUtils.getInstance().toJson(doHandle(handle, aliveList));
    }
}
