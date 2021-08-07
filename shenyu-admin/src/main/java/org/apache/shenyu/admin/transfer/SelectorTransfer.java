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

package org.apache.shenyu.admin.transfer;

import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.common.dto.SelectorData;

/**
 * The interface Plugin transfer.
 */
public enum SelectorTransfer {

    /**
     * The constant INSTANCE.
     */
    INSTANCE;

    /**
     * Map to data selector data.
     *
     * @param selectorDO the selector do
     * @return the selector data
     */
    public SelectorData mapToData(final SelectorDO selectorDO) {
        if (selectorDO == null) {
            return null;
        }
        SelectorData selectorData = new SelectorData();
        selectorData.setId(selectorDO.getId());
        selectorData.setPluginId(selectorDO.getPluginId());
        selectorData.setName(selectorDO.getName());
        selectorData.setMatchMode(selectorDO.getMatchMode());
        selectorData.setType(selectorDO.getType());
        selectorData.setSort(selectorDO.getSort());
        selectorData.setEnabled(selectorDO.getEnabled());
        selectorData.setContinued(selectorDO.getContinued());
        selectorData.setHandle(selectorDO.getHandle());
        return selectorData;
    }

}
