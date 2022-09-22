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

package org.apache.shenyu.admin.listener;

import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.List;

/**
 * AbstractNodeDataChangedListener.
 */
public abstract class AbstractNodeDataChangedListener implements DataChangedListener {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractNodeDataChangedListener.class);

    private final Object ruleSyncObject = new Object();

    private final Object selectorSyncObject = new Object();

    @Override
    public void onAppAuthChanged(final List<AppAuthData> changed, final DataEventTypeEnum eventType) {
        for (AppAuthData data : changed) {
            String appAuthPath = DefaultPathConstants.buildAppAuthPath(data.getAppKey());
            // delete
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteNode(appAuthPath);
                LOG.debug("[DataChangedListener] delete appKey {}", data.getAppKey());
                continue;
            }
            // create or update
            createOrUpdate(appAuthPath, data);
            LOG.debug("[DataChangedListener] change appKey {}", data.getAppKey());
        }
    }

    @Override
    public void onMetaDataChanged(final List<MetaData> changed, final DataEventTypeEnum eventType) {
        for (MetaData data : changed) {
            try {
                String metaDataPath = DefaultPathConstants.buildMetaDataPath(URLEncoder.encode(data.getPath(), "UTF-8"));
                // delete
                if (eventType == DataEventTypeEnum.DELETE) {
                    deleteNode(metaDataPath);
                    LOG.debug("[DataChangedListener] delete appKey {}", metaDataPath);
                    continue;
                }
                // create or update
                createOrUpdate(metaDataPath, data);
                LOG.debug("[DataChangedListener] change metaDataPath {}", metaDataPath);
            } catch (UnsupportedEncodingException e) {
                LOG.error("[DataChangedListener] url encode error.", e);
                throw new ShenyuException(e.getMessage());
            }
        }
    }

    @Override
    public void onSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType) {
        if (eventType == DataEventTypeEnum.REFRESH && !changed.isEmpty()) {
            String selectorParentPath = DefaultPathConstants.buildSelectorParentPath(changed.get(0).getPluginName());
            deletePathRecursive(selectorParentPath);
        }
        for (SelectorData data : changed) {
            String selectorRealPath = DefaultPathConstants.buildSelectorRealPath(data.getPluginName(), data.getId());
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteNode(selectorRealPath);
                LOG.debug("[DataChangedListener] delete appKey {}", selectorRealPath);
                continue;
            }
            //create or update
            synchronized (selectorSyncObject) {
                createOrUpdate(selectorRealPath, data);
            }
            LOG.debug("[DataChangedListener] change path {} with data {}", selectorRealPath, data);
        }
    }

    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType) {
        for (PluginData data : changed) {
            String pluginPath = DefaultPathConstants.buildPluginPath(data.getName());
            // delete
            if (eventType == DataEventTypeEnum.DELETE) {
                deletePathRecursive(pluginPath);
                String selectorParentPath = DefaultPathConstants.buildSelectorParentPath(data.getName());
                deletePathRecursive(selectorParentPath);
                String ruleParentPath = DefaultPathConstants.buildRuleParentPath(data.getName());
                deletePathRecursive(ruleParentPath);
                LOG.debug("[DataChangedListener] delete pluginPath {}", pluginPath);
                continue;
            }
            //create or update
            createOrUpdate(pluginPath, data);
            LOG.debug("[DataChangedListener] change path {} with data {}", pluginPath, data);
        }
    }

    @Override
    public void onRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType) {
        if (eventType == DataEventTypeEnum.REFRESH && !changed.isEmpty()) {
            String selectorParentPath = DefaultPathConstants.buildRuleParentPath(changed.get(0).getPluginName());
            deletePathRecursive(selectorParentPath);
        }
        for (RuleData data : changed) {
            String ruleRealPath = DefaultPathConstants.buildRulePath(data.getPluginName(), data.getSelectorId(), data.getId());
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteNode(ruleRealPath);
                continue;
            }
            //create or update
            synchronized (ruleSyncObject) {
                createOrUpdate(ruleRealPath, data);
            }
            LOG.debug("[DataChangedListener] change path {} with data {}", ruleRealPath, data);
        }
    }

    /**
     * createOrUpdate.
     *
     * @param pluginPath pluginPath
     * @param data data
     */
    public abstract void createOrUpdate(String pluginPath, Object data);

    /**
     * deleteNode.
     *
     * @param pluginPath pluginPath
     */
    public abstract void deleteNode(String pluginPath);

    /**
     * deletePathRecursive.
     *
     * @param selectorParentPath selectorParentPath
     */
    public abstract void deletePathRecursive(String selectorParentPath);
}
