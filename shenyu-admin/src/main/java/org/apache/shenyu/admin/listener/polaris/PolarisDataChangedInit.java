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
import com.tencent.polaris.configuration.api.core.ConfigFileService;
import org.apache.shenyu.admin.config.properties.PolarisProperties;
import org.apache.shenyu.admin.listener.AbstractDataChangedInit;
import org.apache.shenyu.common.constant.PolarisPathConstants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.stream.Stream;

/**
 * The type Polaris data changed init.
 */
public class PolarisDataChangedInit extends AbstractDataChangedInit {

    private static final Logger LOG = LoggerFactory.getLogger(PolarisDataChangedInit.class);

    private final PolarisProperties polarisProperties;

    private final ConfigFileService configFileService;

    /**
     * Instantiates a new Polaris data changed init.
     *
     * @param polarisProperties polarisProperties
     * @param configFileService the configFileService
     */
    public PolarisDataChangedInit(final PolarisProperties polarisProperties, final ConfigFileService configFileService) {
        this.polarisProperties = polarisProperties;
        this.configFileService = configFileService;
    }

    @Override
    protected boolean notExist() {
        return Stream.of(PolarisPathConstants.PLUGIN_DATA_FILE_NAME, PolarisPathConstants.AUTH_DATA_ID_FILE_NAME,
                PolarisPathConstants.META_DATA_FILE_NAME, PolarisPathConstants.PROXY_SELECTOR_FILE_NAME).allMatch(
                this::dataIdNotExist);
    }

    private boolean dataIdNotExist(final String pluginDataId) {
        try {
            return !configFileService.getConfigFile(
                    polarisProperties.getNamespace(),
                    polarisProperties.getFileGroup(),
                    pluginDataId).hasContent();
        } catch (PolarisException e) {
            LOG.error("Get data from polaris error.", e);
            throw new ShenyuException(e.getMessage());
        }
    }
}
