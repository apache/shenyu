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
import org.apache.shenyu.admin.model.dto.PluginNamespaceDTO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.PluginNamespaceQuery;
import org.apache.shenyu.admin.model.query.PluginQuery;
import org.apache.shenyu.admin.model.query.PluginNamespaceQueryCondition;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.PluginNamespaceVO;
import org.apache.shenyu.admin.model.vo.PluginSnapshotVO;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.common.dto.PluginData;

import java.util.List;

/**
 * this is plugin service.
 */
public interface PluginNamespaceService extends PageService<PluginNamespaceQueryCondition, PluginNamespaceVO> {

    /**
     * Create or update string.
     *
     * @param pluginNamespaceDTO the plugin namespace dto
     * @return the string
     */
    String update(PluginNamespaceDTO pluginNamespaceDTO);

    /**
     * Delete string.
     *
     * @param ids the ids
     * @return the string
     */
    String delete(List<String> ids, String namespaceId);

    /**
     * find plugin namespace by id.
     *
     * @param pluginId pk.
     * @return {@linkplain PluginVO}
     */
    PluginNamespaceVO findById(String pluginId,String namespaceId);

    /**
     * find page of plugin namespace by query.
     *
     * @param pluginNamespaceQuery {@linkplain PluginNamespaceQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<PluginNamespaceVO> listByPage(PluginNamespaceQuery pluginNamespaceQuery);

    /**
     * List all list.
     *
     * @return the list
     */
    List<PluginData> listAll(String namespaceId);

    /**
     * List all list.
     *
     * @return the list
     */
    List<PluginData> listAll();

    /**
     * List all vo list.
     *
     * @return the vo list
     */
    List<PluginNamespaceVO> listAllData(String namespaceId);

    /**
     * Enabled string.
     *
     * @param ids     the ids
     * @param enabled the enabled
     * @return the string
     */
    String enabled(List<String> ids, Boolean enabled, String namespaceId);


    /**
     * active plugin snapshot.
     *
     * @return plugin list
     */
    List<PluginSnapshotVO> activePluginSnapshot();

    /**
     * import plugin data.
     *
     * @param pluginList the plugin data
     * @return config import result
     */
    ConfigImportResult importData(List<PluginDTO> pluginList);
}
