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

import org.apache.shenyu.admin.listener.AbstractNodeDataChangedListener;
import org.apache.shenyu.register.client.server.zookeeper.ZookeeperClient;
import org.apache.zookeeper.CreateMode;

/**
 * Use zookeeper to push data changes.
 */
public class ZookeeperDataChangedListener extends AbstractNodeDataChangedListener {

    private final ZookeeperClient zkClient;

    public ZookeeperDataChangedListener(final ZookeeperClient zkClient) {
        this.zkClient = zkClient;
    }

    @Override
    public void createOrUpdate(final String path, final Object data) {
        zkClient.createOrUpdate(path, data, CreateMode.PERSISTENT);
    }

    @Override
    public void deleteNode(final String path) {
        if (zkClient.isExist(path)) {
            zkClient.delete(path);
        }
    }

    @Override
    public void deletePathRecursive(final String path) {
        if (zkClient.isExist(path)) {
            zkClient.delete(path);
        }
    }
}
