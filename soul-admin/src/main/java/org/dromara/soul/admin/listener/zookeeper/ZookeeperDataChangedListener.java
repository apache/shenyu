/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package org.dromara.soul.admin.listener.zookeeper;

import org.I0Itec.zkclient.ZkClient;
import org.dromara.soul.admin.listener.DataChangedListener;
import org.dromara.soul.common.constant.ZkPathConstants;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.MetaData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.enums.DataEventTypeEnum;

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
            // delete
            if (eventType == DataEventTypeEnum.DELETE) {
                String pluginPath = ZkPathConstants.buildAppAuthPath(data.getAppKey());
                if (zkClient.exists(pluginPath)) {
                    zkClient.delete(pluginPath);
                }
                continue;
            }

            // create or update
            String appAuthPath = ZkPathConstants.buildAppAuthPath(data.getAppKey());
            if (!zkClient.exists(appAuthPath)) {
                zkClient.createPersistent(appAuthPath, true);
            }
            zkClient.writeData(appAuthPath, data);
        }
    }

    @Override
    public void onMetaDataChanged(final List<MetaData> changed, final DataEventTypeEnum eventType) {
        for (MetaData data : changed) {
            // delete
            if (eventType == DataEventTypeEnum.DELETE) {
                String path = ZkPathConstants.buildMetaDataPath(data.getAppName(), data.getServiceName(), data.getMethodName());
                if (zkClient.exists(path)) {
                    zkClient.delete(path);
                }
                continue;
            }
            // create or update
            String metaDataPath = ZkPathConstants.buildMetaDataPath(data.getAppName(), data.getServiceName(), data.getMethodName());
            if (!zkClient.exists(metaDataPath)) {
                zkClient.createPersistent(metaDataPath, true);
            }
            zkClient.writeData(metaDataPath, data);
        }
    }

    @Override
    public void onPluginChanged(final List<PluginData> changed, final DataEventTypeEnum eventType) {
        for (PluginData data : changed) {
            // delete
            if (eventType == DataEventTypeEnum.DELETE) {
                String pluginPath = ZkPathConstants.buildPluginPath(data.getName());
                if (zkClient.exists(pluginPath)) {
                    zkClient.deleteRecursive(pluginPath);
                }
                String selectorParentPath = ZkPathConstants.buildSelectorParentPath(data.getName());
                if (zkClient.exists(selectorParentPath)) {
                    zkClient.deleteRecursive(selectorParentPath);
                }
                String ruleParentPath = ZkPathConstants.buildRuleParentPath(data.getName());
                if (zkClient.exists(ruleParentPath)) {
                    zkClient.deleteRecursive(ruleParentPath);
                }
                continue;
            }

            // update
            String pluginPath = ZkPathConstants.buildPluginPath(data.getName());
            if (!zkClient.exists(pluginPath)) {
                zkClient.createPersistent(pluginPath, true);
            }
            zkClient.writeData(pluginPath, data);
        }
    }

    @Override
    public void onSelectorChanged(final List<SelectorData> changed, final DataEventTypeEnum eventType) {
        for (SelectorData data : changed) {
            if (eventType == DataEventTypeEnum.DELETE) {
                String selectorRealPath = ZkPathConstants.buildSelectorRealPath(data.getPluginName(), data.getId());
                if (zkClient.exists(selectorRealPath)) {
                    zkClient.delete(selectorRealPath);
                }
                continue;
            }
            String selectorParentPath = ZkPathConstants.buildSelectorParentPath(data.getPluginName());
            if (!zkClient.exists(selectorParentPath)) {
                zkClient.createPersistent(selectorParentPath, true);
            }
            String selectorRealPath = ZkPathConstants.buildSelectorRealPath(data.getPluginName(), data.getId());
            if (!zkClient.exists(selectorRealPath)) {
                zkClient.createPersistent(selectorRealPath, true);
            }
            zkClient.writeData(selectorRealPath, data);
        }
    }

    @Override
    public void onRuleChanged(final List<RuleData> changed, final DataEventTypeEnum eventType) {
        for (RuleData data : changed) {
            if (eventType == DataEventTypeEnum.DELETE) {
                final String rulePath = ZkPathConstants.buildRulePath(data.getPluginName(), data.getSelectorId(), data.getId());
                if (zkClient.exists(rulePath)) {
                    zkClient.delete(rulePath);
                }
                continue;
            }
            String ruleParentPath = ZkPathConstants.buildRuleParentPath(data.getPluginName());
            if (!zkClient.exists(ruleParentPath)) {
                zkClient.createPersistent(ruleParentPath, true);
            }
            String ruleRealPath = ZkPathConstants.buildRulePath(data.getPluginName(), data.getSelectorId(), data.getId());
            if (!zkClient.exists(ruleRealPath)) {
                zkClient.createPersistent(ruleRealPath, true);
            }
            zkClient.writeData(ruleRealPath, data);
        }
    }

}
