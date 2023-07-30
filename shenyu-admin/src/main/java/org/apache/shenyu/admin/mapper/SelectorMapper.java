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

package org.apache.shenyu.admin.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.query.SelectorQuery;
import org.apache.shenyu.admin.model.query.SelectorQueryCondition;
import org.apache.shenyu.admin.model.vo.SelectorVO;
import org.apache.shenyu.admin.validation.ExistProvider;

import java.io.Serializable;
import java.util.List;
import java.util.Set;

/**
 * SelectorMapper.
 */
@Mapper
public interface SelectorMapper extends ExistProvider {
    
    /**
     * selector existed.
     *
     * @param id id
     * @return existed
     */
    @Override
    Boolean existed(@Param("id") Serializable id);
    
    /**
     * select selector by id.
     *
     * @param id primary key.
     * @return {@linkplain SelectorDO}
     */
    SelectorDO selectById(String id);
    
    /**
     * Select selector by a list of ids.
     *
     * @param idSet a list of ids
     * @return a list of {@linkplain SelectorDO}
     */
    List<SelectorDO> selectByIdSet(@Param("idSet") Set<String> idSet);
    
    /**
     * select selector by query.
     *
     * @param selectorQuery {@linkplain SelectorQuery}
     * @return {@linkplain List}
     */
    List<SelectorDO> selectByQuery(SelectorQuery selectorQuery);
    
    /**
     * Find by plugin id list.
     *
     * @param pluginId the plugin id
     * @return the list
     */
    List<SelectorDO> findByPluginId(String pluginId);
    
    /**
     * Find by plugin id list.
     *
     * @param pluginIds the plugin ids
     * @return the list
     */
    List<SelectorDO> findByPluginIds(List<String> pluginIds);
    
    /**
     * select select by name.
     *
     * @param name the name
     * @return selector do
     */
    SelectorDO selectByName(String name);
    
    /**
     * Find by name and plugin id selector do.
     *
     * @param name     the name
     * @param pluginId the plugin id
     * @return the selector do
     */
    SelectorDO findByNameAndPluginId(@Param("name") String name, @Param("pluginId") String pluginId);
    
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
     * insert selective selector.
     *
     * @param selectorDO {@linkplain SelectorDO}
     * @return rows int
     */
    int insertSelective(SelectorDO selectorDO);
    
    /**
     * update selector.
     *
     * @param selectorDO {@linkplain SelectorDO}
     * @return rows int
     */
    int update(SelectorDO selectorDO);
    
    /**
     * update selective selector.
     *
     * @param selectorDO {@linkplain SelectorDO}
     * @return rows int
     */
    int updateSelective(SelectorDO selectorDO);
    
    /**
     * delete selector.
     *
     * @param id primary key.
     * @return rows int
     */
    int delete(String id);
    
    /**
     * delete selector.
     *
     * @param ids primary keys.
     * @return rows int
     */
    int deleteByIds(List<String> ids);
    
    /**
     * Delete by plugin id int.
     *
     * @param pluginId the plugin id
     * @return the int
     */
    int deleteByPluginId(String pluginId);
    
    /**
     * list all.
     *
     * @return {@linkplain List}
     */
    List<SelectorDO> selectAll();
    
    /**
     * select by condition.
     *
     * @param condition condition
     * @return view data list
     */
    List<SelectorVO> selectByCondition(@Param("condition") SelectorQueryCondition condition);
}
