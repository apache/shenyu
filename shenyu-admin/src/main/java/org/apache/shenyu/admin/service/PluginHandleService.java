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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.PluginHandleDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.PluginHandleQuery;
import org.apache.shenyu.admin.model.vo.PluginHandleVO;

import java.util.List;

/**
 * this is plugin handle service.
 */
public interface PluginHandleService {
    /**
     * find page of plugin handle by query.
     *
     * @param pluginHandleQuery {@linkplain PluginHandleQuery}
     * @return {@link CommonPager}
     */
    CommonPager<PluginHandleVO> listByPage(PluginHandleQuery pluginHandleQuery);
    
    /**
     * create or update plugin handle.
     *
     * @param pluginHandleDTO {@linkplain PluginHandleDTO}
     * @return affected rows
     */
    default Integer createOrUpdate(PluginHandleDTO pluginHandleDTO) {
        return StringUtils.isBlank(pluginHandleDTO.getId()) ? create(pluginHandleDTO) : update(pluginHandleDTO);
    }
    
    /**
     * create.
     *
     * @param pluginHandleDTO param
     * @return changed count
     */
    Integer create(PluginHandleDTO pluginHandleDTO);
    
    /**
     * update.
     *
     * @param pluginHandleDTO param
     * @return changed count
     */
    Integer update(PluginHandleDTO pluginHandleDTO);
    
    /**
     * delete plugin handles.
     *
     * @param ids ids to delete
     * @return The number of rows deleted
     */
    Integer deletePluginHandles(List<String> ids);
    
    /**
     * find plugin handle by id.
     *
     * @param id plugin handle id.
     * @return {@linkplain PluginHandleVO}
     */
    PluginHandleVO findById(String id);
    
    /**
     * find plugin handle list by plugin id.
     *
     * @param pluginId the plugin id.
     * @param type     type 1:selector,2:rule
     * @return plugin handle list.
     */
    List<PluginHandleVO> list(String pluginId, Integer type);
}
