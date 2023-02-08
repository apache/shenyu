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
import com.alibaba.nacos.api.exception.NacosException;
import org.apache.shenyu.admin.listener.AbstractDataChangedInit;
import org.apache.shenyu.common.constant.NacosPathConstants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Objects;
import java.util.stream.Stream;

/**
 * The type Nacos data changed init.
 *
 * @since 2.5.0
 */
public class NacosDataChangedInit extends AbstractDataChangedInit {

    private static final Logger LOG = LoggerFactory.getLogger(NacosDataChangedInit.class);

    private final ConfigService configService;

    /**
     * Instantiates a new Nacos data changed init.
     *
     * @param configService the configService
     */
    public NacosDataChangedInit(final ConfigService configService) {
        this.configService = configService;
    }

    @Override
    protected boolean notExist() {
        return Stream.of(NacosPathConstants.PLUGIN_DATA_ID, NacosPathConstants.AUTH_DATA_ID, NacosPathConstants.META_DATA_ID).allMatch(
            this::dataIdNotExist);
    }

    private boolean dataIdNotExist(final String pluginDataId) {
        try {
            return Objects.isNull(
                    configService.getConfig(pluginDataId,
                            NacosPathConstants.GROUP,
                            NacosPathConstants.DEFAULT_TIME_OUT));
        } catch (NacosException e) {
            LOG.error("Get data from nacos error.", e);
            throw new ShenyuException(e.getMessage());
        }
    }
}
