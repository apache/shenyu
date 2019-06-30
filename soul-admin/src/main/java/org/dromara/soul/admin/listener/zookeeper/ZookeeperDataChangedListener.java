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
import org.dromara.soul.admin.listener.AbstractDataChangedListener;
import org.dromara.soul.admin.listener.DataEventType;
import org.dromara.soul.common.constant.ZkPathConstants;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;

import java.util.List;

/**
 * Use zookeeper to push data changes.
 * @author huangxiaofeng
 * @date 2019/6/30 0:02
 */
public class ZookeeperDataChangedListener extends AbstractDataChangedListener {

    private final ZkClient zkClient;

    public ZookeeperDataChangedListener(ZkClient zkClient) {
        this.zkClient = zkClient;
    }

    @Override
    protected void afterAppAuthChanged(List<AppAuthData> changed, DataEventType eventType) {
        for (AppAuthData data : changed) {
            // delete
            if ( eventType == DataEventType.DELETE ) {
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
            zkClient.writeData(appAuthPath, new AppAuthData(data.getAppKey(), data.getAppSecret(), data.getEnabled()));
        }
    }

    @Override
    protected void afterPluginChanged(List<PluginData> changed, DataEventType eventType) {
        for (PluginData data : changed) {
            // delete
            if ( eventType == DataEventType.DELETE ) {
                String pluginPath = ZkPathConstants.buildPluginPath(data.getName());
                if (zkClient.exists(pluginPath)) {
                    zkClient.delete(pluginPath);
                }
                continue;
            }

            // update
            String pluginPath = ZkPathConstants.buildPluginPath(data.getName());
            if (!zkClient.exists(pluginPath)) {
                zkClient.createPersistent(pluginPath, true);
            }
            zkClient.writeData(pluginPath, new PluginData(data.getId(),
                    data.getName(), data.getRole(), data.getEnabled()));
        }
    }

    @Override
    protected void afterRuleChanged(List<RuleData> changed, DataEventType eventType) {

    }

    @Override
    protected void afterSelectorChanged(List<SelectorData> changed, DataEventType eventType) {

    }

}
