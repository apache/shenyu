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

package org.apache.shenyu.client.apache.dubbo.processor.extractor;

import org.apache.commons.lang3.StringUtils;
import org.apache.dubbo.common.constants.CommonConstants;
import org.apache.dubbo.config.annotation.DubboService;
import org.apache.dubbo.config.spring.ServiceBean;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.matcher.ApiAnnotationProcessor;
import org.apache.shenyu.client.core.register.matcher.ExtractorProcessor;
import org.apache.shenyu.client.dubbo.common.dto.DubboRpcExt;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.ListUtil;

import java.util.List;
import java.util.Optional;

import static org.apache.dubbo.remoting.Constants.DEFAULT_CONNECT_TIMEOUT;

/**
 * DubboServiceProcessor.
 */
public class DubboServiceProcessor implements ApiAnnotationProcessor<DubboService>, ExtractorProcessor {
    
    @Override
    public List<String> supportedClient() {
        return ListUtil.of(RpcTypeEnum.DUBBO.getName());
    }
    
    @Override
    public void process(final ApiBean apiBean, final DubboService annotation) {
        apiBean.setBeanPath(annotation.path());
        
        apiBean.addProperties("rpcExt", getRpcExt(apiBean));
    }
    
    @Override
    public void process(final ApiBean.ApiDefinition definition) {
        definition.addProperties("rpcExt", getRpcExt(definition));
    }
    
    @Override
    public void process(final ApiBean.ApiDefinition definition, final DubboService annotation) {
        // nothing
    }
    
    @Override
    public Class<DubboService> matchAnnotation() {
        return DubboService.class;
    }
    
    private String getRpcExt(final ApiBean apiBean) {
        final Object beanInstance = apiBean.getBeanInstance();
        if (beanInstance instanceof ServiceBean) {
            return getRpcExt((ServiceBean<?>) beanInstance);
        }
        return "{}";
    }
    
    private String getRpcExt(final ApiBean.ApiDefinition definition) {
        final Object beanInstance = definition.getApiBean().getBeanInstance();
        if (beanInstance instanceof ServiceBean) {
            return getRpcExt((ServiceBean<?>) beanInstance);
        }
        return "{}";
    }
    
    private static String getRpcExt(final ServiceBean<?> serviceBean) {
        DubboRpcExt build = DubboRpcExt.builder()
                .protocol(StringUtils.isNotEmpty(serviceBean.getProtocol().getName()) ? serviceBean.getProtocol().getName() : "")
                .group(StringUtils.isNotEmpty(serviceBean.getGroup()) ? serviceBean.getGroup() : "")
                .version(StringUtils.isNotEmpty(serviceBean.getVersion()) ? serviceBean.getVersion() : "")
                .loadbalance(StringUtils.isNotEmpty(serviceBean.getLoadbalance()) ? serviceBean.getLoadbalance() : CommonConstants.DEFAULT_LOADBALANCE)
                .retries(Optional.ofNullable(serviceBean.getRetries()).orElse(CommonConstants.DEFAULT_RETRIES))
                .timeout(Optional.ofNullable(serviceBean.getTimeout()).orElse(DEFAULT_CONNECT_TIMEOUT))
                .sent(Optional.ofNullable(serviceBean.getSent()).orElse(Boolean.FALSE))
                .cluster(StringUtils.isNotEmpty(serviceBean.getCluster()) ? serviceBean.getCluster() : Constants.DEFAULT_CLUSTER)
                .url("")
                .build();
        return GsonUtils.getInstance().toJson(build);
    }
}
