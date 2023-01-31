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
import org.apache.shenyu.admin.listener.AbstractDataChangedInit;
import org.apache.shenyu.common.constant.ConsulConstants;

import java.util.Objects;

/**
 * The type Consul data changed init.
 *
 * @since 2.5.0
 */
public class ConsulDataChangedInit extends AbstractDataChangedInit {

    private final ConsulClient consulClient;

    /**
     * Instantiates a new Consul data changed init.
     *
     * @param consulClient the Consul client
     */
    public ConsulDataChangedInit(final ConsulClient consulClient) {
        this.consulClient = consulClient;
    }

    @Override
    protected boolean notExist() {
        return dataKeyNotExist(ConsulConstants.PLUGIN_DATA)
                && dataKeyNotExist(ConsulConstants.AUTH_DATA)
                && dataKeyNotExist(ConsulConstants.META_DATA);
    }

    private boolean dataKeyNotExist(final String dataKey) {
        return Objects.isNull(consulClient.getKVValue(dataKey).getValue());
    }
}
