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

package org.apache.shenyu.admin.listener.polaris;

import com.tencent.polaris.api.exception.PolarisException;
import com.tencent.polaris.configuration.api.core.ConfigFile;
import com.tencent.polaris.configuration.api.core.ConfigFilePublishService;
import com.tencent.polaris.configuration.api.core.ConfigFileService;
import com.tencent.polaris.configuration.client.internal.DefaultConfigFileMetadata;
import org.apache.shenyu.admin.config.properties.PolarisProperties;
import org.apache.shenyu.admin.listener.AbstractNodeDataChangedListener;
import org.apache.shenyu.common.constant.NacosPathConstants;
import org.apache.shenyu.common.constant.PolarisPathConstants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;

/**
 * Use polaris to push data changes.
 */
public class PolarisDataChangedListener extends AbstractNodeDataChangedListener {

    private static final Logger LOG = LoggerFactory.getLogger(PolarisDataChangedListener.class);

    private final PolarisProperties polarisProperties;

    private final ConfigFileService configFileService;

    private final ConfigFilePublishService configFilePublishService;

    public PolarisDataChangedListener(final PolarisProperties polarisProperties, final ConfigFileService configFileService, final ConfigFilePublishService configFilePublishService) {
        super(new ChangeData(PolarisPathConstants.PLUGIN_DATA_FILE_NAME, PolarisPathConstants.SELECTOR_DATA_FILE_NAME,
                PolarisPathConstants.RULE_DATA_FILE_NAME, PolarisPathConstants.AUTH_DATA_ID_FILE_NAME,
                PolarisPathConstants.META_DATA_FILE_NAME, PolarisPathConstants.PROXY_SELECTOR_FILE_NAME, NacosPathConstants.DISCOVERY_DATA_ID));
        this.polarisProperties = polarisProperties;
        this.configFileService = configFileService;
        this.configFilePublishService = configFilePublishService;
    }

    @Override
    public void doPublishConfig(final String dataId, final Object data) {
        try {
            DefaultConfigFileMetadata metadata = new DefaultConfigFileMetadata(
                    polarisProperties.getNamespace(),
                    polarisProperties.getFileGroup(),
                    dataId);
            if (isReleased(metadata)) {
                configFilePublishService.updateConfigFile(metadata, Objects.isNull(data) ? "" : GsonUtils.getInstance().toJson(data));
            } else {
                configFilePublishService.createConfigFile(metadata, GsonUtils.getInstance().toJson(data));
            }
            configFilePublishService.releaseConfigFile(metadata);
        } catch (PolarisException e) {
            LOG.error("Polaris Publish data to polaris error.", e);
        }
    }

    @Override
    public void doDelConfig(final String dataId) {
        doPublishConfig(dataId, null);
    }

    @Override
    public String getConfig(final String dataId) {
        try {
            ConfigFile configFile = configFileService.getConfigFile(polarisProperties.getNamespace(), polarisProperties.getFileGroup(), dataId);
            return configFile.hasContent() ? configFile.getContent() : null;
        } catch (PolarisException e) {
            LOG.error("Polaris Get data from polaris error.", e);
            throw new ShenyuException(e.getMessage());
        }
    }

    private boolean isReleased(final DefaultConfigFileMetadata metadata) {
        try {
            return Objects.nonNull(configFileService.getConfigFile(metadata).getContent());
        } catch (PolarisException e) {
            LOG.error("Polaris Get data from polaris error.", e);
            throw new ShenyuException(e.getMessage());
        }
    }
}
