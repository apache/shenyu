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

import org.apache.shenyu.admin.model.dto.NamespacePluginDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.NamespacePluginQuery;
import org.apache.shenyu.admin.model.query.NamespacePluginQueryCondition;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
import org.apache.shenyu.admin.model.vo.PluginSnapshotVO;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.admin.service.configs.ConfigsImportContext;
import org.apache.shenyu.common.dto.PluginData;

import java.util.List;

/**
 * this is plugin service.
 */
public interface NamespacePluginService extends PageService<NamespacePluginQueryCondition, NamespacePluginVO> {
    
    /**
     * find by id.
     *
     * @param id primary key.
     * @return {@linkplain PluginVO}
     */
    NamespacePluginVO findById(String id);
    
    
    /**
     * find by namespaceId and pluginId.
     *
     * @param namespaceId namespaceId.
     * @param pluginId    pluginId.
     * @return {@linkplain NamespacePluginVO}
     */
    NamespacePluginVO findByNamespaceIdAndPluginId(String namespaceId, String pluginId);

    /**
     * Update string.
     *
     * @param namespaceId        namespaceId.
     * @param pluginId           pluginId.
     * @return the string
     */
    NamespacePluginVO create(String namespaceId, String pluginId);

    /**
     * Create string.
     *
     * @param namespacePluginDTO the plugin namespace dto
     * @return the string
     */
    String update(NamespacePluginDTO namespacePluginDTO);

    /**
     * Delete namespace plugin relations by ids.
     *
     * @param ids         the ids.
     * @return the string
     */
    String delete(List<String> ids);

    /**
     * find page of plugin namespace by query.
     *
     * @param namespacePluginQuery {@linkplain NamespacePluginQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<NamespacePluginVO> listByPage(NamespacePluginQuery namespacePluginQuery);

    /**
     * List all list.
     *
     * @param namespaceId the namespaceId
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
     * List by namespace id list.
     *
     * @param namespaceId the namespaceId
     * @return the list
     */
    List<NamespacePluginVO> listByNamespaceId(String namespaceId);
    
    /**
     * List all vo list.
     *
     * @param namespaceId the namespace id
     * @return the vo list
     */
    List<NamespacePluginVO> listAllData(String namespaceId);

    /**
     * Enabled string.
     *
     * @param ids         the ids
     * @param enabled     the enabled
     * @return the string
     */
    String enabled(List<String> ids, Boolean enabled);

    /**
     * Enabled string.
     *
     * @param namespaceId the namespaceId
     * @param pluginIds   the pluginIds
     * @param enabled     the enabled
     * @return the string
     */
    String enabled(String namespaceId, List<String> pluginIds, Boolean enabled);


    /**
     * active plugin snapshot.
     *
     * @param namespaceId the namespaceId
     * @return plugin list
     */
    List<PluginSnapshotVO> activePluginSnapshot(String namespaceId);

    /**
     * import plugin data.
     *
     * @param namespace  the namespace
     * @param pluginList the plugin data
     * @param context the import context
     * @return config import result
     */
    ConfigImportResult importData(String namespace, List<NamespacePluginDTO> pluginList, ConfigsImportContext context);
    
    /**
     * List by namespace.
     *
     * @param namespace the namespace
     * @return the list
     */
    List<PluginData> listByNamespace(String namespace);
}
