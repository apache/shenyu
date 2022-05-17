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
import org.apache.shenyu.admin.model.entity.PluginHandleDO;
import org.apache.shenyu.admin.model.query.PluginHandleQuery;
import org.apache.shenyu.admin.validation.ExistProvider;

import java.io.Serializable;
import java.util.List;

/**
 * The interface Plugin handle mapper.
 */
@Mapper
public interface PluginHandleMapper extends ExistProvider {
    
    /**
     * plugin existed.
     *
     * @param id id
     * @return existed
     */
    @Override
    Boolean existed(@Param("id") Serializable id);
    
    /**
     * Select plugin handle by id.
     *
     * @param id the id.
     * @return the plugin handle do.
     */
    PluginHandleDO selectById(@Param("id") String id);
    
    /**
     * find plugin handle do list by plugin id.
     *
     * @param pluginId the pluginId
     * @return the list
     */
    List<PluginHandleDO> findByPluginId(@Param("pluginId") String pluginId);
    
    /**
     * insert plugin handle.
     *
     * @param record {@link PluginHandleDO}
     * @return affected rows
     */
    int insert(PluginHandleDO record);
    
    /**
     * insert selective plugin handle.
     *
     * @param record {@link PluginHandleDO}
     * @return affected rows.
     */
    int insertSelective(PluginHandleDO record);
    
    /**
     * count plugin handle by query.
     *
     * @param pluginHandleQuery {@linkplain PluginHandleQuery}
     * @return the count
     */
    int countByQuery(PluginHandleQuery pluginHandleQuery);
    
    /**
     * select plugin handle list by query.
     *
     * @param pluginHandleQuery {@linkplain PluginHandleQuery}
     * @return the plugin handle list
     */
    List<PluginHandleDO> selectByQuery(PluginHandleQuery pluginHandleQuery);
    
    /**
     * bach delete by idList.
     *
     * @param ids a list of ids
     * @return the count of deleted
     */
    List<PluginHandleDO> selectByIdList(@Param("ids") List<String> ids);
    
    
    /**
     * update some selective columns in plugin_handle.
     *
     * @param record {@linkplain PluginHandleDO}
     * @return affected rows
     */
    int updateByPrimaryKeySelective(PluginHandleDO record);
    
    /**
     * update plugin handle by primary key.
     *
     * @param record {@linkplain PluginHandleDO}
     * @return affected rows.
     */
    int updateByPrimaryKey(PluginHandleDO record);
    
    /**
     * delete string id.
     *
     * @param id plugin handle id
     * @return affected rows
     */
    int delete(String id);
    
    /**
     * bach delete by idList.
     *
     * @param idList a list of ids
     * @return the count of deleted
     */
    int deleteByIdList(@Param("idList") List<String> idList);
    
    
    /**
     * bach delete by idList.
     *
     * @param pluginId a list of ids
     * @return the count of deleted
     */
    List<PluginHandleDO> selectByPluginIdList(@Param("pluginIds") List<String> pluginId);
    
    /**
     * delete string id.
     *
     * @param pluginIds plugin ids
     * @return affected rows
     */
    int deleteByPluginIds(List<String> pluginIds);
}
