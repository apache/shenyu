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
import com.ecwid.consul.v1.Response;
import com.ecwid.consul.v1.kv.model.GetValue;
import org.apache.shenyu.admin.listener.AbstractListDataChangedListener;
import org.apache.shenyu.common.constant.ConsulConstants;
import org.apache.shenyu.common.utils.GsonUtils;

import java.util.Objects;

/**
 *  Use Consul to push data changes.
 */
public class ConsulDataChangedListener extends AbstractListDataChangedListener {
    private final ConsulClient consulClient;

    public ConsulDataChangedListener(final ConsulClient consulClient) {
        super(new ChangeData(ConsulConstants.PLUGIN_DATA, ConsulConstants.SELECTOR_DATA,
                ConsulConstants.RULE_DATA, ConsulConstants.AUTH_DATA, ConsulConstants.META_DATA));
        this.consulClient = consulClient;
    }

    @Override
    public void publishConfig(final String dataKey, final Object data) {
        consulClient.setKVValue(dataKey, GsonUtils.getInstance().toJson(data));
    }

    @Override
    public String getConfig(final String dataKey) {
        Response<GetValue> kvValue = consulClient.getKVValue(dataKey);
        return Objects.nonNull(kvValue.getValue()) ? kvValue.getValue().getDecodedValue() : ConsulConstants.EMPTY_CONFIG_DEFAULT_VALUE;
    }
}
