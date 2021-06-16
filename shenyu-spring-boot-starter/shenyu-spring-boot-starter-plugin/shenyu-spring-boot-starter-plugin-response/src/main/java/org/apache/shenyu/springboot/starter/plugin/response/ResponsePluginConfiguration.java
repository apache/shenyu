/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.apache.shenyu.springboot.starter.plugin.response;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.response.ResponsePlugin;
import org.apache.shenyu.plugin.response.config.HttpClientProperties;
import org.apache.shenyu.plugin.response.strategy.ResponseHandler;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The type response plugin configuration.
 */
@Configuration
@EnableConfigurationProperties(HttpClientProperties.class)
public class ResponsePluginConfiguration {

    /**
     * response plugin shenyu plugin.
     *
     * @return the shenyu plugin.
     */
    @Bean
    public ShenyuPlugin responsePlugin() {
        return new ResponsePlugin();
    }

    /**
     * response handler.
     *
     * @return the response handler.
     */
    @Bean
    public ResponseHandler responseHandler() {
        return new ResponseHandler();
    }
}
