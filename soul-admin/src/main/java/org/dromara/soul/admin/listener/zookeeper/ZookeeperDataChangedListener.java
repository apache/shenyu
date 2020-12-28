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

package org.dromara.soul.admin.listener.zookeeper;

import lombok.SneakyThrows;
import org.I0Itec.zkclient.ZkClient;
import org.dromara.soul.admin.listener.DataChangedListener;
import org.dromara.soul.common.constant.ZkPathConstants;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.DataEventTypeEnum;

import java.net.URLEncoder;
import java.util.List;

/**
 * Use zookeeper to push data changes.
 *
 * @author huangxiaofeng
 * @author xiaoyu
 */
public class ZookeeperDataChangedListener implements DataChangedListener {

    private final ZkClient zkClient;

    public ZookeeperDataChangedListener(final ZkClient zkClient) {
        this.zkClient = zkClient;
    }

    @Override
    public void onAppAuthChanged(final List<AppAuthData> changed, final DataEventTypeEnum eventType) {
        for (AppAuthData data : changed) {
            final String appAuthPath = ZkPathConstants.buildAppAuthPath(data.getAppKey());
            // delete
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteZkPath(appAuthPath);
                continue;
            }
            // create or update
            upsertZkNode(appAuthPath, data);
        }
    }

    @SneakyThrows
    @Override
    public void onMetaDataChanged(final List<MetaData> changed, final DataEventTypeEnum eventType) {
        for (MetaData data : changed) {
            final String metaDataPath = ZkPathConstants.buildMetaDataPath(URLEncoder.encode(data.getPath(), "UTF-8"));
            // delete
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteZkPath(metaDataPath);
                continue;
            }
            // create or update
            upsertZkNode(metaDataPath, data);
        }
    }

    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType) {
        for (PluginData data : changed) {
            final String pluginPath = ZkPathConstants.buildPluginPath(data.getName());
            // delete
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteZkPathRecursive(pluginPath);
                final String selectorParentPath = ZkPathConstants.buildSelectorParentPath(data.getName());
                deleteZkPathRecursive(selectorParentPath);
                final String ruleParentPath = ZkPathConstants.buildRuleParentPath(data.getName());
                deleteZkPathRecursive(ruleParentPath);
                continue;
            }
            //create or update
            upsertZkNode(pluginPath, data);
        }
    }

    @Override
    public void onSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType) {
        if (eventType == DataEventTypeEnum.REFRESH) {
            final String selectorParentPath = ZkPathConstants.buildSelectorParentPath(changed.get(0).getPluginName());
            deleteZkPathRecursive(selectorParentPath);
        }
        for (SelectorData data : changed) {
            final String selectorRealPath = ZkPathConstants.buildSelectorRealPath(data.getPluginName(), data.getId());
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteZkPath(selectorRealPath);
                continue;
            }
            final String selectorParentPath = ZkPathConstants.buildSelectorParentPath(data.getPluginName());
            createZkNode(selectorParentPath);
            //create or update
            upsertZkNode(selectorRealPath, data);
        }
    }

    @Override
    public void onRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType) {
        if (eventType == DataEventTypeEnum.REFRESH) {
            final String selectorParentPath = ZkPathConstants.buildRuleParentPath(changed.get(0).getPluginName());
            deleteZkPathRecursive(selectorParentPath);
        }
        for (RuleData data : changed) {
            final String ruleRealPath = ZkPathConstants.buildRulePath(data.getPluginName(), data.getSelectorId(), data.getId());
            if (eventType == DataEventTypeEnum.DELETE) {
                deleteZkPath(ruleRealPath);
                continue;
            }
            final String ruleParentPath = ZkPathConstants.buildRuleParentPath(data.getPluginName());
            createZkNode(ruleParentPath);
            //create or update
            upsertZkNode(ruleRealPath, data);
        }
    }
    
    private void createZkNode(final String path) {
        if (!zkClient.exists(path)) {
            zkClient.createPersistent(path, true);
        }
    }
    
    /**
     * create or update zookeeper node.
     * @param path node path
     * @param data node data 
     */
    private void upsertZkNode(final String path, final Object data) {
        if (!zkClient.exists(path)) {
            zkClient.createPersistent(path, true);
        }
        zkClient.writeData(path, data);
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
