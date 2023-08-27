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

package org.apache.shenyu.admin.listener.nacos;

import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.api.config.ConfigType;
import com.alibaba.nacos.api.exception.NacosException;
import org.apache.shenyu.admin.listener.AbstractNodeDataChangedListener;
import org.apache.shenyu.common.constant.NacosPathConstants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Use nacos to push data changes.
 */
public class NacosDataChangedListener extends AbstractNodeDataChangedListener {

    private static final Logger LOG = LoggerFactory.getLogger(NacosDataChangedListener.class);

    private final ConfigService configService;

    public NacosDataChangedListener(final ConfigService configService) {
        super(new ChangeData(NacosPathConstants.PLUGIN_DATA_ID, NacosPathConstants.SELECTOR_DATA_ID,
                NacosPathConstants.RULE_DATA_ID, NacosPathConstants.AUTH_DATA_ID, NacosPathConstants.META_DATA_ID,
                NacosPathConstants.PROXY_SELECTOR_DATA_ID, NacosPathConstants.DISCOVERY_DATA_ID));
        this.configService = configService;
    }

    @Override
    public void doPublishConfig(final String dataId, final Object data) {
        try {
            configService.publishConfig(
                    dataId, 
                    NacosPathConstants.GROUP, 
                    GsonUtils.getInstance().toJson(data),
                    ConfigType.JSON.getType());
        } catch (NacosException e) {
            LOG.error("Publish data to nacos error.", e);
            throw new ShenyuException(e.getMessage());
        }
    }

    @Override
    public void doDelConfig(final String dataId) {
        try {
            configService.removeConfig(
                    dataId,
                    NacosPathConstants.GROUP);
        } catch (NacosException e) {
            LOG.error("Publish data to nacos error.", e);
            throw new ShenyuException(e.getMessage());
        }
    }

    @Override
    public String getConfig(final String dataId) {
        try {
            return configService.getConfig(dataId, NacosPathConstants.GROUP, NacosPathConstants.DEFAULT_TIME_OUT);
        } catch (NacosException e) {
            LOG.error("Get data from nacos error.", e);
            throw new ShenyuException(e.getMessage());
        }
    }

}
