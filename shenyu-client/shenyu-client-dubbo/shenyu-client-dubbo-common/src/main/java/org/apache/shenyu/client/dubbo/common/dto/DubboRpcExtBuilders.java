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

package org.apache.shenyu.client.dubbo.common.dto;

import org.apache.commons.lang3.StringUtils;
import org.apache.dubbo.common.constants.CommonConstants;
import org.apache.dubbo.config.MethodConfig;
import org.apache.dubbo.config.spring.ServiceBean;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.GsonUtils;

import java.util.ArrayList;
import java.util.Objects;
import java.util.Optional;

import static org.apache.dubbo.remoting.Constants.DEFAULT_CONNECT_TIMEOUT;

public final class DubboRpcExtBuilders {

    private DubboRpcExtBuilders() {
    }

    public static String buildRpcExt(final ServiceBean<?> serviceBean) {
        DubboRpcExt build = DubboRpcExt.builder()
                .protocol(Objects.nonNull(serviceBean.getProtocol()) && StringUtils.isNotEmpty(serviceBean.getProtocol().getName()) ? serviceBean.getProtocol().getName() : "")
                .group(StringUtils.isNotEmpty(serviceBean.getGroup()) ? serviceBean.getGroup() : "")
                .version(StringUtils.isNotEmpty(serviceBean.getVersion()) ? serviceBean.getVersion() : "")
                .loadbalance(StringUtils.isNotEmpty(serviceBean.getLoadbalance()) ? serviceBean.getLoadbalance() : CommonConstants.DEFAULT_LOADBALANCE)
                .retries(Optional.ofNullable(serviceBean.getRetries()).orElse(CommonConstants.DEFAULT_RETRIES))
                .timeout(Optional.ofNullable(serviceBean.getTimeout()).orElse(DEFAULT_CONNECT_TIMEOUT))
                .sent(Optional.ofNullable(serviceBean.getSent()).orElse(Boolean.FALSE))
                .cluster(StringUtils.isNotEmpty(serviceBean.getCluster()) ? serviceBean.getCluster() : Constants.DEFAULT_CLUSTER)
                .url("")
                .serialization(serviceBean.getSerialization())
                .build();
        if (Objects.nonNull(serviceBean.getMethods())) {
            build.setMethods(new ArrayList<>());
            for (MethodConfig methodConfig : serviceBean.getMethods()) {
                DubboRpcMethodExt methodExt = new DubboRpcMethodExt();
                methodExt.setName(methodConfig.getName());
                methodExt.setLoadbalance(methodConfig.getLoadbalance());
                methodExt.setRetries(methodConfig.getRetries());
                methodExt.setTimeout(methodConfig.getTimeout());
                methodExt.setSent(methodConfig.getSent());
                build.getMethods().add(methodExt);
            }
        }
        return GsonUtils.getInstance().toJson(build);
    }
}
