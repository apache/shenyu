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

package org.apache.shenyu.client.springmvc.register;

import com.google.common.collect.Lists;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.parser.ApiMetaDefinitionParser;
import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.PathUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

public class SpringMvcApiMetaDefinitionParser implements ApiMetaDefinitionParser<Object> {

    private final Boolean addPrefixed;

    private final String appName;

    public SpringMvcApiMetaDefinitionParser(final Boolean addPrefixed, final String appName) {
        this.addPrefixed = addPrefixed;
        this.appName = appName;
    }

    @Override
    public List<MetaDataRegisterDTO> parse(final ApiBean<Object>.ApiDefinition apiDefinition) {

        ShenyuSpringMvcClient methodAnnotation = apiDefinition.getAnnotation(ShenyuSpringMvcClient.class);

        String methodPath = Objects.isNull(methodAnnotation) ? StringUtils.EMPTY : methodAnnotation.path();

        if (StringUtils.isEmpty(methodPath)) {
            methodPath = apiDefinition.getMethodPath();
        }

        String apiPath = PathUtils.pathJoin(apiDefinition.getContextPath(), apiDefinition.getBeanPath(), methodPath);

        String parameterTypes = Optional.ofNullable(apiDefinition.getApiMethod())
                .map(m -> Arrays.stream(m.getParameterTypes()).map(Class::getName)
                        .collect(Collectors.joining(","))).orElse(null);

        ShenyuSpringMvcClient classAnnotation = apiDefinition.getApiBean()
                .getAnnotation(ShenyuSpringMvcClient.class);

        String pathDesc = Objects.isNull(methodAnnotation) ? classAnnotation.desc() : methodAnnotation.desc();

        boolean enabled = (Objects.isNull(classAnnotation) || classAnnotation.enabled())
                && (Objects.isNull(methodAnnotation) || methodAnnotation.enabled());

        String ruleName = Objects.isNull(methodAnnotation) || StringUtils.isEmpty(methodAnnotation.ruleName())
                ? apiPath : methodAnnotation.ruleName();

        boolean registerMetaData = (Objects.isNull(classAnnotation) || classAnnotation.registerMetaData())
                && (Objects.isNull(methodAnnotation) || methodAnnotation.registerMetaData());

        return Lists.newArrayList(MetaDataRegisterDTO.builder()
                .contextPath(apiDefinition.getContextPath())
                .addPrefixed(addPrefixed)
                .appName(appName)
                .serviceName(apiDefinition.getBeanClass().getName())
                .methodName(apiDefinition.getApiMethodName())
                .path(apiPath)
                .pathDesc(pathDesc)
                .parameterTypes(parameterTypes)
                .rpcType(RpcTypeEnum.HTTP.getName())
                .enabled(enabled)
                .ruleName(ruleName)
                .registerMetaData(registerMetaData)
                .build());
    }
}
