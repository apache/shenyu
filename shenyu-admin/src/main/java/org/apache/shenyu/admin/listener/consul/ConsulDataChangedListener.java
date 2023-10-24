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

package org.apache.shenyu.admin.listener.consul;

import com.ecwid.consul.v1.ConsulClient;
import org.apache.shenyu.admin.listener.AbstractPathDataChangedListener;
import org.apache.shenyu.common.utils.GsonUtils;

/**
 *  Use Consul to push data changes.
 */
public class ConsulDataChangedListener extends AbstractPathDataChangedListener {
    private final ConsulClient consulClient;

    public ConsulDataChangedListener(final ConsulClient consulClient) {
        this.consulClient = consulClient;
    }

    @Override
    public void createOrUpdate(final String pluginPath, final Object data) {
        consulClient.setKVValue(pluginPath, GsonUtils.getInstance().toJson(data));
    }

    @Override
    public void deleteNode(final String pluginPath) {
        consulClient.deleteKVValue(pluginPath);
    }

    @Override
    public void deletePathRecursive(final String selectorParentPath) {
        consulClient.deleteKVValues(selectorParentPath);
    }
}
