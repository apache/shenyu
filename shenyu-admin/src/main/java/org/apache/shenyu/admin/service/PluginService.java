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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.PluginQuery;
import org.apache.shenyu.admin.model.query.PluginQueryCondition;
import org.apache.shenyu.admin.model.vo.PluginSnapshotVO;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.common.dto.PluginData;

import java.util.List;

/**
 * this is plugin service.
 */
public interface PluginService extends PageService<PluginQueryCondition, PluginVO> {
    
    /**
     * Create or update string.
     *
     * @param pluginDTO the plugin dto
     * @return the string
     */
    String createOrUpdate(PluginDTO pluginDTO);
    
    /**
     * Delete string.
     *
     * @param ids the ids
     * @return the string
     */
    String delete(List<String> ids);
    
    /**
     * find plugin by id.
     *
     * @param id pk.
     * @return {@linkplain PluginVO}
     */
    PluginVO findById(String id);
    
    /**
     * find page of plugin by query.
     *
     * @param pluginQuery {@linkplain PluginQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<PluginVO> listByPage(PluginQuery pluginQuery);
    
    /**
     * List all list.
     *
     * @return the list
     */
    List<PluginData> listAll();
    
    /**
     * list all not in resource.
     *
     * @return the list
     */
    List<PluginData> listAllNotInResource();
    
    /**
     * Enabled string.
     *
     * @param ids     the ids
     * @param enabled the enable
     * @return the string
     */
    String enabled(List<String> ids, Boolean enabled);
    
    /**
     * select Plugin's id by name.
     *
     * @param name the plugin's name.
     * @return the id of Plugin.
     */
    String selectIdByName(String name);
    
    /**
     * Find by name plugin do.
     *
     * @param name the name
     * @return the plugin do
     */
    PluginDO findByName(String name);
    
    /**
     * active plugin snapshot.
     *
     * @return plugin list
     */
    List<PluginSnapshotVO> activePluginSnapshot();
}
