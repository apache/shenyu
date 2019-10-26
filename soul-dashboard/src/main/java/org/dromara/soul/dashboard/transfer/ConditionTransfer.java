/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.dashboard.transfer;


import org.dromara.soul.common.dto.ConditionData;
import org.dromara.soul.dashboard.dto.SelectorConditionDTO;
import org.dromara.soul.dashboard.entity.SelectorConditionDO;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

/**
 * The interface Condition transfer.
 *
 * @author xiaoyu(Myth)
 */
@Mapper
public interface ConditionTransfer {

    /**
     * The constant INSTANCE.
     */
    ConditionTransfer INSTANCE = Mappers.getMapper(ConditionTransfer.class);


    /**
     * Map to selector data condition data.
     *
     * @param selectorConditionDO the selector condition do
     * @return the condition data
     */
    ConditionData mapToSelectorDO(SelectorConditionDO selectorConditionDO);

    /**
     * Map to selector data dto condition data.
     *
     * @param selectorConditionDTO the selector condition dto
     * @return the condition data
     */
    ConditionData mapToSelectorDTO(SelectorConditionDTO selectorConditionDTO);


}
