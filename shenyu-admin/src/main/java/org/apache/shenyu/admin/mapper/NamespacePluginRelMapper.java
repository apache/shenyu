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
import org.apache.shenyu.admin.model.entity.NamespacePluginRelDO;
import org.apache.shenyu.admin.model.query.NamespacePluginQuery;
import org.apache.shenyu.admin.model.query.NamespacePluginQueryCondition;
import org.apache.shenyu.admin.model.query.PluginQuery;
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.admin.validation.ExistProvider;

import java.io.Serializable;
import java.util.List;

/**
 * NamespacePluginRelMapper.
 */
@Mapper
public interface NamespacePluginRelMapper extends ExistProvider {
    
    /**
     * select namespace plugin by id.
     *
     * @param id primary key
     * @return NamespacePluginVO
     */
    NamespacePluginVO selectById(String id);

    /**
     * existed.
     *
     * @param id id
     * @return existed
     */
    @Override
    Boolean existed(@Param("id") Serializable id);

    /**
     * Batch save int.
     *
     * @param namespacePluginRelDOList the pluginNsRel do list
     * @return the int
     */
    int batchSave(@Param("namespacePluginRelDOList") List<NamespacePluginRelDO> namespacePluginRelDOList);

    /**
     * select plugin by query.
     *
     * @param namespacePluginQuery {@linkplain PluginQuery}
     * @return {@linkplain List}
     */
    List<NamespacePluginVO> selectByQuery(NamespacePluginQuery namespacePluginQuery);

    /**
     * select plugin by PluginId and namespaceId.
     *
     * @param pluginId    primary key.
     * @param namespaceId namespaceId.
     * @return {@linkplain PluginVO}
     */
    NamespacePluginVO selectByPluginIdAndNamespaceId(String pluginId, String namespaceId);

    /**
     * select plugin by pluginId.
     *
     * @param pluginId    primary key.
     * @return {@linkplain PluginVO}
     */
    NamespacePluginVO selectByPluginId(String pluginId);

    /**
     * search by condition.
     *
     * @param condition condition.
     * @return list
     */
    List<NamespacePluginVO> searchByCondition(@Param("condition") NamespacePluginQueryCondition condition);

    /**
     * plugin existed.
     *
     * @param name    name
     * @param exclude exclude
     * @param namespaceId namespaceId.
     * @return existed
     */
    Boolean nameExistedExclude(@Param("name") Serializable name, @Param("exclude") List<String> exclude, @Param("namespaceId") String namespaceId);
    
    /**
     * update selective plugin.
     *
     * @param namespacePluginRelDO {@linkplain NamespacePluginRelDO}
     * @return rows int
     */
    int updateSelective(NamespacePluginRelDO namespacePluginRelDO);
    
    /**
     * update plugin by namespaceId and pluginId.
     *
     * @param namespacePluginRelDO {@linkplain NamespacePluginRelDO}
     * @return rows int
     */
    int updateByNamespaceIdAndPluginId(NamespacePluginRelDO namespacePluginRelDO);

    /**
     * select plugin by ids.
     *
     * @param ids   primary keys.
     * @return {@linkplain NamespacePluginRelDO}
     */
    List<NamespacePluginVO> selectByIds(List<String> ids);

    /**
     * select plugin by namespaceId and pluginIds.
     *
     * @param namespaceId namespaceId.
     * @param pluginIds   pluginIds.
     * @return {@linkplain NamespacePluginRelDO}
     */
    List<NamespacePluginVO> selectByNamespaceIdAndPluginIds(String namespaceId, List<String> pluginIds);


    /**
     * delete plugin by ids and namespaceId.
     *
     * @param ids         primary keys.
     * @return rows int
     */
    int deleteByIds(List<String> ids);
    
    /**
     * list by namespaceId.
     *
     * @param namespaceId namespaceId.
     * @return {@linkplain List}
     */
    List<NamespacePluginRelDO> listByNamespaceId(String namespaceId);

    /**
     * select by namespaceId.
     *
     * @param namespaceId namespaceId.
     * @return {@linkplain List}
     */
    List<NamespacePluginVO> selectByNamespaceId(String namespaceId);

    /**
     * select all by namespaceId.
     *
     * @param namespaceId namespaceId.
     * @return {@linkplain List}
     */
    List<NamespacePluginVO> selectAllByNamespaceId(String namespaceId);
  
    /**
     * select all by namespaceIds.
     *
     * @param namespaceIds namespaceIds.
     * @return {@linkplain List}
     */
    List<NamespacePluginVO> selectAllByNamespaceIds(List<String> namespaceIds);

    /**
     * select all.
     *
     * @return {@linkplain List}
     */
    List<NamespacePluginVO> selectAll();

    /**
     * enable data by a list of ids.
     *
     * @param idList  a list of ids
     * @param enabled status
     * @return the count of enabled datas
     */
    int updateEnableByIdList(@Param("idList") List<String> idList, @Param("enabled") Boolean enabled);

    /**
     * enable data by a list of ids.
     *
     * @param namespaceId namespaceId
     * @param pluginIds a list of pluginIds
     * @param enabled status
     * @return the count of enabled datas
     */
    int updateEnableByNamespaceIdAndPluginIdList(@Param("namespaceId") String namespaceId, @Param("pluginIds") List<String> pluginIds, @Param("enabled") Boolean enabled);

    /**
     * insert selective plugin.
     *
     * @param namespacePluginRelDO {@linkplain NamespacePluginRelDO}
     * @return rows int
     */
    int insertSelective(NamespacePluginRelDO namespacePluginRelDO);
}
