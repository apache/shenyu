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

package org.apache.shenyu.admin.listener.zookeeper;

import org.I0Itec.zkclient.ZkClient;
import org.apache.shenyu.admin.listener.DataChangedListener;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.List;

/**
 * Use zookeeper to push data changes.
 */
public class ZookeeperDataChangedListener implements DataChangedListener {

    private static final Logger LOG = LoggerFactory.getLogger(ZookeeperDataChangedListener.class);

    private final ZkClient zkClient;

    public ZookeeperDataChangedListener(final ZkClient zkClient) {
        this.zkClient = zkClient;
    }

    @Override
    public void onAppAuthChanged(final List<AppAuthData> changed, final DataEventTypeEnum eventType) {
        for (AppAuthData data : changed) {
            String appAuthPath = DefaultPathConstants.buildAppAuthPath(data.getAppKey());
            // delete
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteZkPath(appAuthPath);
                continue;
            }
            // create or update
            insertZkNode(appAuthPath, data);
        }
    }

    @Override
    public void onMetaDataChanged(final List<MetaData> changed, final DataEventTypeEnum eventType) {
        for (MetaData data : changed) {
            try {
                String metaDataPath = DefaultPathConstants.buildMetaDataPath(URLEncoder.encode(data.getPath(), "UTF-8"));
                // delete
                if (eventType == DataEventTypeEnum.DELETE) {
                    deleteZkPath(metaDataPath);
                    continue;
                }
                // create or update
                insertZkNode(metaDataPath, data);
            } catch (UnsupportedEncodingException e) {
                LOG.error("Url encode error.", e);
                throw new ShenyuException(e.getMessage());
            }
        }
    }

    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType) {
        for (PluginData data : changed) {
            String pluginPath = DefaultPathConstants.buildPluginPath(data.getName());
            // delete
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteZkPathRecursive(pluginPath);
                String selectorParentPath = DefaultPathConstants.buildSelectorParentPath(data.getName());
                deleteZkPathRecursive(selectorParentPath);
                String ruleParentPath = DefaultPathConstants.buildRuleParentPath(data.getName());
                deleteZkPathRecursive(ruleParentPath);
                continue;
            }
            //create or update
            insertZkNode(pluginPath, data);
        }
    }

    @Override
    public void onSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType) {
        if (eventType == DataEventTypeEnum.REFRESH && !changed.isEmpty()) {
            String selectorParentPath = DefaultPathConstants.buildSelectorParentPath(changed.get(0).getPluginName());
            deleteZkPathRecursive(selectorParentPath);
        }
        for (SelectorData data : changed) {
            String selectorRealPath = DefaultPathConstants.buildSelectorRealPath(data.getPluginName(), data.getId());
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteZkPath(selectorRealPath);
                continue;
            }
            String selectorParentPath = DefaultPathConstants.buildSelectorParentPath(data.getPluginName());
            createZkNode(selectorParentPath);
            //create or update
            insertZkNode(selectorRealPath, data);
        }
    }

    @Override
    public void onRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType) {
        if (eventType == DataEventTypeEnum.REFRESH && !changed.isEmpty()) {
            String selectorParentPath = DefaultPathConstants.buildRuleParentPath(changed.get(0).getPluginName());
            deleteZkPathRecursive(selectorParentPath);
        }
        for (RuleData data : changed) {
            String ruleRealPath = DefaultPathConstants.buildRulePath(data.getPluginName(), data.getSelectorId(), data.getId());
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteZkPath(ruleRealPath);
                continue;
            }
            String ruleParentPath = DefaultPathConstants.buildRuleParentPath(data.getPluginName());
            createZkNode(ruleParentPath);
            //create or update
            insertZkNode(ruleRealPath, data);
        }
    }
    
    private void insertZkNode(final String path, final Object data) {
        createZkNode(path);
        zkClient.writeData(path, null == data ? "" : GsonUtils.getInstance().toJson(data));
    }
    
    private void createZkNode(final String path) {
        if (!zkClient.exists(path)) {
            zkClient.createPersistent(path, true);
        }
    }
    
    private void deleteZkPath(final String path) {
        if (zkClient.exists(path)) {
            zkClient.delete(path);
        }
    }
    
    private void deleteZkPathRecursive(final String path) { 
        if (zkClient.exists(path)) {
            zkClient.deleteRecursive(path);
        }
    }
}
