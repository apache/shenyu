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

package org.apache.shenyu.admin.listener.etcd;

import org.apache.shenyu.admin.listener.AbstractNodeDataChangedListener;
import org.apache.shenyu.common.utils.GsonUtils;

/**
 * EtcdDataDataChangedListener.
 */
public class EtcdDataDataChangedListener extends AbstractNodeDataChangedListener {

    private final EtcdClient etcdClient;

    public EtcdDataDataChangedListener(final EtcdClient client) {
        this.etcdClient = client;
    }

    @Override
    public void createOrUpdate(final String pluginPath, final Object data) {
        etcdClient.put(pluginPath, GsonUtils.getInstance().toJson(data));
    }

    @Override
    public void deleteNode(final String pluginPath) {
        etcdClient.delete(pluginPath);
    }

    @Override
    public void deletePathRecursive(final String selectorParentPath) {
        etcdClient.deleteEtcdPathRecursive(selectorParentPath);
    }
}
