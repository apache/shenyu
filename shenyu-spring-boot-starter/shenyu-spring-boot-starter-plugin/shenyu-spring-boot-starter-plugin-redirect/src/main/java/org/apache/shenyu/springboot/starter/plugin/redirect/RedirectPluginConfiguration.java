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

package org.apache.shenyu.springboot.starter.plugin.redirect;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.redirect.RedirectPlugin;
import org.apache.shenyu.plugin.redirect.handler.RedirectPluginDataHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.DispatcherHandler;

/**
 * the type redirect plugin Configuration.
 */
@Configuration
@ConditionalOnProperty(value = {"shenyu.plugins.redirect.enabled"}, havingValue = "true", matchIfMissing = true)
public class RedirectPluginConfiguration {

    /**
     * Redirect plugin.
     *
     * @param dispatcherHandler {@link DispatcherHandler}
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin redirectPlugin(final DispatcherHandler dispatcherHandler) {
        return new RedirectPlugin(dispatcherHandler);
    }

    /**
     * Redirect plugin data handler.
     *
     * @return the plugin data handler
     */
    @Bean
    public PluginDataHandler redirectPluginDataHandler() {
        return new RedirectPluginDataHandler();
    }
}
