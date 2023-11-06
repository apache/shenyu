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

package org.apache.shenyu.admin.config;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.security.SecurityScheme.In;
import io.swagger.v3.oas.models.security.SecurityScheme.Type;
import org.apache.shenyu.common.utils.VersionUtils;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Configuration class for Swagger API document.
 */
@Configuration
public class SwaggerConfiguration {

    private static final String DEFAULT_SWAGGER_API_VERSION = "2.3.0";

    private static final String TITLE = "ShenYu Admin API Document";

    private static final String DESCRIPTION = "ShenYu Admin API";

    private static final String CONTACT_NAME = "ShenYu";

    private static final String CONTACT_URL = "https://github.com/apache/shenyu";

    private static final String CONTACT_EMAIL = "dev@shenyu.apache.org";

    private static final String TOKEN_DESCRIPTION = "user auth";

    public SwaggerConfiguration() {
    }

    /**
     * Fetch version information from pom.xml and set title, version, description,
     * contact for Swagger API document.
     *
     * @return Api info
     */
    @Bean
    public OpenAPI apiInfo() {
        return new OpenAPI()
                .info(new Info()
                        .title(TITLE).description(DESCRIPTION)
                        .version(VersionUtils.getVersion(getClass(), DEFAULT_SWAGGER_API_VERSION))
                        .contact(new Contact().name(CONTACT_NAME).url(CONTACT_URL).email(CONTACT_EMAIL))
                )
                .components(new Components()
                        .addSecuritySchemes(org.apache.shenyu.common.constant.Constants.X_ACCESS_TOKEN, new SecurityScheme()
                                .name(org.apache.shenyu.common.constant.Constants.X_ACCESS_TOKEN)
                                .type(Type.APIKEY)
                                .in(In.HEADER)
                                .description(TOKEN_DESCRIPTION)
                        )
                ).addSecurityItem(new SecurityRequirement().addList(org.apache.shenyu.common.constant.Constants.X_ACCESS_TOKEN));
    }
}
