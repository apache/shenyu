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

package org.dromara.soul.dashboard.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.dromara.soul.dashboard.entity.SelectorConditionDO;
import org.dromara.soul.dashboard.query.SelectorConditionQuery;

import java.util.List;

/**
 * SelectorConditionMapper.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@Mapper
public interface SelectorConditionMapper {

    /**
     * select selector condition by id.
     *
     * @param id primary key.
     * @return {@linkplain SelectorConditionDO}
     */
    SelectorConditionDO selectById(String id);

    /**
     * select selector condition by query.
     *
     * @param selectorConditionQuery {@linkplain SelectorConditionQuery}
     * @return {@linkplain List}
     */
    List<SelectorConditionDO> selectPageByQuery(SelectorConditionQuery selectorConditionQuery);

    /**
     * insert selector condition.
     *
     * @param selectorConditionDO {@linkplain SelectorConditionDO}
     * @return rows int
     */
    int insert(SelectorConditionDO selectorConditionDO);

    /**
     * update selector condition.
     *
     * @param selectorConditionDO {@linkplain SelectorConditionDO}
     * @return rows int
     */
    int update(SelectorConditionDO selectorConditionDO);

    /**
     * delete selector condition by id.
     *
     * @param id primary key.
     * @return rows int
     */
    int delete(String id);

    /**
     * Delete by selector id int.
     *
     * @param selectorId the selector id
     * @return the int
     */
    int deleteBySelectorId(String selectorId);
}
