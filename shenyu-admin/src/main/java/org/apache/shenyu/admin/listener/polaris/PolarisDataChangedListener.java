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
import org.apache.shenyu.admin.listener.AbstractListDataChangedListener;
import org.apache.shenyu.common.constant.NacosPathConstants;
import org.apache.shenyu.common.constant.PolarisPathConstants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Use polaris to push data changes.
 */
public class PolarisDataChangedListener extends AbstractListDataChangedListener {

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
    public void publishConfig(final String dataId, final Object data) {
        try {
            DefaultConfigFileMetadata metadata = new DefaultConfigFileMetadata(
                    polarisProperties.getNamespace(),
                    polarisProperties.getFileGroup(),
                    dataId);
            if (isReleased(dataId)) {
                configFilePublishService.updateConfigFile(metadata, GsonUtils.getInstance().toJson(data));
            } else {
                configFilePublishService.createConfigFile(metadata, GsonUtils.getInstance().toJson(data));
            }
            configFilePublishService.releaseConfigFile(metadata);
        } catch (PolarisException e) {
            LOG.error("Publish data to polaris error.", e);
            throw new ShenyuException(e.getMessage());
        }
    }

    @Override
    public String getConfig(final String dataId) {
        try {
            ConfigFile configFile = configFileService.getConfigFile(polarisProperties.getNamespace(), polarisProperties.getFileGroup(), dataId);
            return configFile.hasContent() ? configFile.getContent() : PolarisPathConstants.EMPTY_CONFIG_DEFAULT_VALUE;
        } catch (PolarisException e) {
            LOG.error("Get data from polaris error.", e);
            throw new ShenyuException(e.getMessage());
        }
    }

    private boolean isReleased(final String pluginDataId) {
        try {
            return configFileService.getConfigFile(
                    polarisProperties.getNamespace(),
                    polarisProperties.getFileGroup(),
                    pluginDataId).hasContent();
        } catch (PolarisException e) {
            LOG.error("Get data from polaris error.", e);
            throw new ShenyuException(e.getMessage());
        }
    }
}
