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
import org.dromara.soul.dashboard.entity.SelectorDO;
import org.dromara.soul.dashboard.query.SelectorQuery;

import java.util.List;

/**
 * SelectorMapper.
 *
 * @author xiaoyu
 */
@Mapper
public interface SelectorMapper {

    /**
     * select selector by id.
     *
     * @param id primary key.
     * @return {@linkplain SelectorDO}
     */
    SelectorDO selectById(String id);

    /**
     * select select by name
     *
     * @param name the name
     * @return selector do
     */
    SelectorDO selectByName(String name);

    /**
     * select selector by query.
     *
     * @param selectorQuery {@linkplain SelectorQuery}
     * @return {@linkplain List}
     */
    List<SelectorDO> selectPageByQuery(SelectorQuery selectorQuery);


    /**
     * Find by plugin id list.
     *
     * @param pluginId the plugin id
     * @return the list
     */
    List<SelectorDO> findByPluginId(String pluginId);

    /**
     * count selector by query.
     *
     * @param selectorQuery {@linkplain SelectorQuery}
     * @return {@linkplain Integer}
     */
    Integer countByQuery(SelectorQuery selectorQuery);

    /**
     * insert selector.
     *
     * @param selectorDO {@linkplain SelectorDO}
     * @return rows int
     */
    int insert(SelectorDO selectorDO);


    /**
     * update selector.
     *
     * @param selectorDO {@linkplain SelectorDO}
     * @return rows int
     */
    int update(SelectorDO selectorDO);

    /**
     * delete selector.
     *
     * @param id primary key.
     * @return rows int
     */
    int delete(String id);

    /**
     * Delete by plugin id int.
     *
     * @param pluginId the plugin id
     * @return the int
     */
    int deleteByPluginId(String pluginId);

    /**
     * list all {@linkplain SelectorDO}
     *
     * @return {@linkplain List}
     */
    List<SelectorDO> selectAll();

}
