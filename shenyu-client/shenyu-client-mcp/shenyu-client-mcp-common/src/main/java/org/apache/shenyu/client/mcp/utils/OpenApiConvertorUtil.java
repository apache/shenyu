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

package org.apache.shenyu.client.mcp.utils;

import io.swagger.v3.oas.models.Operation;
import io.swagger.v3.oas.models.parameters.Parameter;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpHeader;
import org.apache.shenyu.client.mcp.common.annotation.ShenyuMcpRequestConfig;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * OpenApiConvertorUtil.
 */
public class OpenApiConvertorUtil {

    public static io.swagger.v3.oas.models.parameters.Parameter convertParameter(final io.swagger.v3.oas.annotations.Parameter annotation) {

        io.swagger.v3.oas.models.parameters.Parameter parameter = new io.swagger.v3.oas.models.parameters.Parameter();

        if (!Objects.nonNull(annotation)) {
            return parameter;
        }

        parameter.setName(annotation.name());
        parameter.setIn(annotation.in().toString().toLowerCase());
        parameter.setDescription(annotation.description());
        parameter.setRequired(annotation.required());

        io.swagger.v3.oas.annotations.media.Schema schemaAnn = annotation.schema();
        if (!schemaAnn.implementation().equals(Void.class)
                || StringUtils.isNoneBlank(schemaAnn.type())
                || StringUtils.isNoneBlank(schemaAnn.format())) {
            io.swagger.v3.oas.models.media.Schema<Object> schema = new io.swagger.v3.oas.models.media.Schema<>();
            if (StringUtils.isNoneBlank(schemaAnn.type())) {
                schema.setType(schemaAnn.type());
            }
            parameter.setSchema(schema);
        }
        return parameter;
    }

    public static Operation convertOperation(final io.swagger.v3.oas.annotations.Operation operationAnnotation) {
        Operation operation = new Operation();

        if (Objects.isNull(operationAnnotation)) {
            return operation;
        }

        operation.setSummary(operationAnnotation.summary());
        operation.setDescription(operationAnnotation.description());
        operation.setDeprecated(operationAnnotation.deprecated());
        operation.setOperationId(operationAnnotation.operationId());

        // Tags
        if (Objects.nonNull(operationAnnotation.tags()) && operationAnnotation.tags().length > 0) {
            operation.setTags(Arrays.asList(operationAnnotation.tags()));
        }

        // Parameters
        if (Objects.nonNull(operationAnnotation.parameters()) && operationAnnotation.parameters().length > 0) {
            List<Parameter> parameters = Arrays.stream(operationAnnotation.parameters())
                    .map(OpenApiConvertorUtil::convertParameter)
                    .collect(Collectors.toList());
            operation.setParameters(parameters);
        }

        return operation;
    }

    public static org.apache.shenyu.client.mcp.common.dto.ShenyuMcpRequestConfig convertRequestConfig(final ShenyuMcpRequestConfig requestConfigAnnotation) {
        org.apache.shenyu.client.mcp.common.dto.ShenyuMcpRequestConfig requestConfigObject = new org.apache.shenyu.client.mcp.common.dto.ShenyuMcpRequestConfig();
        ShenyuMcpHeader[] headersAnnotation = requestConfigAnnotation.headers();
        Map<String, String> headersObject = requestConfigObject.getHeaders();
        for (ShenyuMcpHeader shenyuMcpHeader : headersAnnotation) {
            headersObject.put(shenyuMcpHeader.key(), shenyuMcpHeader.value());
        }
        requestConfigObject.setBodyToJson(requestConfigAnnotation.bodyToJson());
        return requestConfigObject;
    }
}
