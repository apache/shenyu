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

package org.apache.shenyu.examples.http.config;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.media.StringSchema;
import io.swagger.v3.oas.models.parameters.Parameter;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.tags.Tag;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.VersionUtils;
import org.springdoc.core.models.GroupedOpenApi;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.List;

/**
 * Configuration class for Swagger API document using Springdoc.
 */
@Configuration
public class SwaggerConfiguration {

    private static final String DEFAULT_SWAGGER_API_VERSION = "3.0.1";

    private static final String TITLE = "shenyu-examples-http-swagger3 API";

    private static final String DESCRIPTION = "shenyu-examples-http-swagger3 API";

    private static final String CONTACT_NAME = "ShenYu";

    private static final String CONTACT_URL = "https://github.com/apache/shenyu";

    private static final String CONTACT_EMAIL = "dev@shenyu.apache.org";

    private static final String TOKEN_DESCRIPTION = "user auth";

    private static final String TOKEN_PARAMETER_TYPE = "header";

    public SwaggerConfiguration() {
    }

    /**
     * Configure The OpenAPI with Springdoc.
     *
     * @return GroupedOpenApi {@linkplain GroupedOpenApi}
     */
    @Bean
    public GroupedOpenApi createRestApi() {
        Parameter commonParam = new Parameter()
                .in(TOKEN_PARAMETER_TYPE)
                .name(Constants.X_ACCESS_TOKEN)
                .description(TOKEN_DESCRIPTION)
                .required(false)
                .schema(new StringSchema());

        return GroupedOpenApi.builder()
                .group("default")
                .addOperationCustomizer((operation, handlerMethod) -> {
                    operation.addParametersItem(commonParam);
                    return operation;
                })
                .packagesToScan("org.apache.shenyu.examples.http.controller")
                .build();
    }

    /**
     * Configure the OpenAPI with API information.
     *
     * @return OpenAPI {@linkplain OpenAPI}
     */
    @Bean
    public OpenAPI customOpenAPI() {
        return new OpenAPI()
                .info(new Info()
                        .title(TITLE)
                        .version(VersionUtils.getVersion(getClass(), DEFAULT_SWAGGER_API_VERSION))
                        .description(DESCRIPTION)
                        .contact(new io.swagger.v3.oas.models.info.Contact()
                                .name(CONTACT_NAME)
                                .url(CONTACT_URL)
                                .email(CONTACT_EMAIL)))
                .components(new Components()
                        .addSecuritySchemes(Constants.X_ACCESS_TOKEN, new SecurityScheme()
                                .type(SecurityScheme.Type.APIKEY)
                                .in(SecurityScheme.In.HEADER)
                                .name(Constants.X_ACCESS_TOKEN)))
                .addSecurityItem(new SecurityRequirement().addList(Constants.X_ACCESS_TOKEN))
                .tags(List.of(
                        new Tag().name("RequestController"),
                        new Tag().name("Order API")
                ));
    }
}
