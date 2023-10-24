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

package org.apache.shenyu.plugin.logging.elasticsearch;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.logging.common.AbstractLoggingPlugin;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.elasticsearch.collector.ElasticSearchLogCollector;
import org.springframework.web.server.ServerWebExchange;

/**
 * Integrated elasticsearch collect log.
 */
public class LoggingElasticSearchPlugin extends AbstractLoggingPlugin<ShenyuRequestLog> {

    @Override
    protected LogCollector<ShenyuRequestLog> logCollector() {
        return ElasticSearchLogCollector.getInstance();
    }

    /**
     * pluginEnum.
     *
     * @return plugin
     */
    @Override
    public PluginEnum pluginEnum() {
        return PluginEnum.LOGGING_ELASTIC_SEARCH;
    }

    /**
     * log collect extension.
     * base on ShenyuRequestLog to extend log
     *
     * @param exchange exchange
     * @param selector selector
     * @param rule rule
     * @return base ShenyuRequestLog
     */
    @Override
    protected ShenyuRequestLog doLogExecute(final ServerWebExchange exchange, final SelectorData selector, final RuleData rule) {
        return new ShenyuRequestLog();
    }
}
