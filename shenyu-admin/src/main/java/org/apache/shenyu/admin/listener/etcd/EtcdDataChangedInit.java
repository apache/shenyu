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

import org.apache.shenyu.admin.listener.AbstractDataChangedInit;
import org.apache.shenyu.common.constant.DefaultPathConstants;

/**
 * The type Etcd data changed init.
 *
 * @since 2.5.0
 */
public class EtcdDataChangedInit extends AbstractDataChangedInit {

    private final EtcdClient etcdClient;

    /**
     * Instantiates a new Etcd data changed init.
     *
     * @param etcdClient the etcdClient client
     */
    public EtcdDataChangedInit(final EtcdClient etcdClient) {
        this.etcdClient = etcdClient;
    }

    @Override
    protected boolean notExist() {
        return !etcdClient.exists(DefaultPathConstants.PLUGIN_PARENT)
                && !etcdClient.exists(DefaultPathConstants.APP_AUTH_PARENT)
                && !etcdClient.exists(DefaultPathConstants.META_DATA);
    }
}
