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

import org.apache.shenyu.admin.listener.AbstractDataChangedInit;
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.register.client.server.zookeeper.ZookeeperClient;

import java.util.stream.Stream;

/**
 * The type Zookeeper data changed init.
 *
 * @since 2.5.0
 */
public class ZookeeperDataChangedInit extends AbstractDataChangedInit {

    private final ZookeeperClient zkClient;

    /**
     * Instantiates a new Zookeeper data changed init.
     *
     * @param zkClient        the zk client
     */
    public ZookeeperDataChangedInit(final ZookeeperClient zkClient) {
        this.zkClient = zkClient;
    }

    @Override
    protected boolean notExist() {
        return Stream.of(DefaultPathConstants.PLUGIN_PARENT, DefaultPathConstants.APP_AUTH_PARENT, DefaultPathConstants.META_DATA).noneMatch(zkClient::isExist);
    }
}
