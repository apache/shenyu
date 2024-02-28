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

package org.apache.shenyu.client.core.register.registrar;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.common.utils.PathUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.jetbrains.annotations.NotNull;

/**
 * MateDataApiRegistrarImplImpl.
 *
 * @see MetaDataRegisterDTO
 */
public class MateDataApiRegistrarImpl extends BaseApiRegistrarImpl {
    
    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();
    
    private final ClientRegisterConfig clientRegisterConfig;
    
    public MateDataApiRegistrarImpl(final ClientRegisterConfig clientRegisterConfig) {
        this.clientRegisterConfig = clientRegisterConfig;
    }
    
    @Override
    public Class<?> registerDataType() {
        return MetaDataRegisterDTO.class;
    }
    
    @Override
    protected void doRegisterApi(final ApiBean.ApiDefinition api) {
        final MetaDataRegisterDTO metaDataRegisterDTO = MetaDataRegisterDTO.builder()
                .contextPath(clientRegisterConfig.getContextPath())
                .addPrefixed(clientRegisterConfig.getAddPrefixed())
                .appName(clientRegisterConfig.getAppName())
                .serviceName(api.getBeanClass().getName())
                .host(clientRegisterConfig.getHost())
                .port(clientRegisterConfig.getPort())
                .methodName(api.getApiMethodName())
                .path(buildPath(api))
                .pathDesc(buildDesc(api))
                .parameterTypes(buildParameterTypes(api))
                .rpcType(api.getApiBean().getClientName())
                .registerMetaData(getRegisterMetaData(api))
                .enabled(getEnable(api))
                .ruleName(buildRule(api))
                .build();
        
        register(metaDataRegisterDTO);
    }
    
    @Override
    protected void doRegisterBean(final ApiBean apiBean) {
        final MetaDataRegisterDTO metaDataRegisterDTO = MetaDataRegisterDTO.builder()
                .contextPath(clientRegisterConfig.getContextPath())
                .addPrefixed(clientRegisterConfig.getAddPrefixed())
                .appName(clientRegisterConfig.getAppName())
                .serviceName(apiBean.getBeanClass().getName())
                .host(clientRegisterConfig.getHost())
                .port(clientRegisterConfig.getPort())
                .methodName(apiBean.getBeanClass().getSimpleName())
                .path(buildPath(apiBean))
                .pathDesc(buildDesc(apiBean))
                .parameterTypes(buildParameterTypes(apiBean))
                .rpcType(apiBean.getClientName())
                .enabled(getEnable(apiBean))
                .registerMetaData(getRegisterMetaData(apiBean))
                .ruleName(buildRule(apiBean))
                .build();
        
        register(metaDataRegisterDTO);
    }
    
    private String buildParameterTypes(final ApiBean api) {
        final String parameterTypes = api.getPropertiesValue("parameterTypes");
        if (StringUtils.isNotBlank(parameterTypes)) {
            return parameterTypes;
        }
        return null;
    }
    
    private String buildParameterTypes(final ApiBean.ApiDefinition api) {
        final String parameterTypes = api.getPropertiesValue("parameterTypes");
        if (StringUtils.isNotBlank(parameterTypes)) {
            return parameterTypes;
        }
        return null;
    }
    
    private String buildRule(@NotNull final ApiBean api) {
        final String rule = api.getPropertiesValue("rule");
        if (StringUtils.isNotBlank(rule)) {
            return rule;
        }
        return StringUtils.defaultIfBlank("", buildPath(api));
    }
    
    private String buildRule(@NotNull final ApiBean.ApiDefinition api) {
        final String rule = api.getPropertiesValue("rule");
        if (StringUtils.isNotBlank(rule)) {
            return rule;
        }
        return StringUtils.defaultIfBlank("", buildPath(api));
    }
    
    private boolean getEnable(final ApiBean api) {
        final String enable = api.getPropertiesValue("enable");
        if (StringUtils.isNotBlank(enable)) {
            return Boolean.parseBoolean(enable);
        }
        return true;
    }
    
    private boolean getEnable(final ApiBean.ApiDefinition api) {
        final String enable = api.getPropertiesValue("enable");
        if (StringUtils.isNotBlank(enable)) {
            return Boolean.parseBoolean(enable);
        }
        return true;
    }

    private boolean getRegisterMetaData(final ApiBean api) {
        final String enable = api.getPropertiesValue("registerMetaData");
        if (StringUtils.isNotBlank(enable)) {
            return Boolean.parseBoolean(enable);
        }
        return true;
    }

    private boolean getRegisterMetaData(final ApiBean.ApiDefinition api) {
        final String enable = api.getPropertiesValue("registerMetaData");
        if (StringUtils.isNotBlank(enable)) {
            return Boolean.parseBoolean(enable);
        }
        return true;
    }
    
    private String buildDesc(final ApiBean api) {
        final String desc = api.getPropertiesValue("desc");
        if (StringUtils.isNotBlank(desc)) {
            return desc;
        }
        return api.getClientName()
                + ":"
                + api.getBeanClass().getName()
                + "#*";
    }
    
    private String buildDesc(final ApiBean.ApiDefinition api) {
        final String desc = api.getPropertiesValue("desc");
        if (StringUtils.isNotBlank(desc)) {
            return desc;
        }
        return api.getApiBean().getClientName()
                + ":"
                + api.getApiBean().getBeanClass().getName()
                + "#"
                + api.getApiMethod().getName();
    }
    
    private String buildPath(final ApiBean api) {
        return PathUtils.pathJoin(clientRegisterConfig.getContextPath(), api.getBeanPath());
    }
    
    private String buildPath(final ApiBean.ApiDefinition api) {
        return PathUtils.pathJoin(clientRegisterConfig.getContextPath(), api.getApiBean().getBeanPath(), api.getMethodPath());
    }
    
    private void register(final MetaDataRegisterDTO metaDataRegisterDTO) {
        publisher.publishEvent(metaDataRegisterDTO);
    }
}
