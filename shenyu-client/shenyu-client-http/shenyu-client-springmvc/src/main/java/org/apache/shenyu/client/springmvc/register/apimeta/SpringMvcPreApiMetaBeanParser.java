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

package org.apache.shenyu.client.springmvc.register.apimeta;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.register.parser.PreApiMetaBeanParser;
import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.PathUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;

public class SpringMvcPreApiMetaBeanParser implements PreApiMetaBeanParser {

    private final Boolean addPrefixed;

    private final String appName;

    private final String host;

    private final Integer port;

    public SpringMvcPreApiMetaBeanParser(final ClientRegisterConfig clientRegisterConfig) {
        this.addPrefixed = clientRegisterConfig.getAddPrefixed();
        this.appName = clientRegisterConfig.getAppName();
        this.host = clientRegisterConfig.getHost();
        this.port = clientRegisterConfig.getPort();
    }

    @Override
    public MetaDataRegisterDTO parse(final ApiBean apiBean) {
        return apiBean2ApiMeta(apiBean);
    }

    private MetaDataRegisterDTO apiBean2ApiMeta(final ApiBean apiBean) {

        ShenyuSpringMvcClient annotation = apiBean.getAnnotation(ShenyuSpringMvcClient.class);
        String apiPath = PathUtils.pathJoin(apiBean.getContextPath(), annotation.path());

        return MetaDataRegisterDTO.builder()
                .contextPath(apiBean.getContextPath())
                .addPrefixed(addPrefixed)
                .appName(appName)
                .serviceName(apiBean.getBeanClass().getName())
                .host(host)
                .port(port)
                .methodName(null)
                .path(apiPath)
                .pathDesc(annotation.desc())
                .parameterTypes(null)
                .rpcType(RpcTypeEnum.HTTP.getName())
                .enabled(annotation.enabled())
                .ruleName(StringUtils.defaultIfBlank(annotation.ruleName(), apiPath))
                .registerMetaData(annotation.registerMetaData())
                .build();
    }

}
