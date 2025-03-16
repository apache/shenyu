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

package org.apache.shenyu.plugin.ai.prompt;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.AiPromptHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.ai.prompt.handler.AiPromptPluginDataHandler;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * Ai Prompt Plugin.
 */
public class AiPromptPlugin extends AbstractShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AiPromptPlugin.class);
    
    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        
        AiPromptHandle aiPromptHandle = AiPromptPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        if (Objects.isNull(aiPromptHandle)) {
            LOG.info("ai prompt rule can not configuration");
            return chain.execute(exchange);
        }
        
        return chain.execute(exchange);
    }

    @Override
    public String named() {
        return PluginEnum.AI_PROMPT.getName();
    }

    @Override
    public int getOrder() {
        return PluginEnum.AI_PROMPT.getCode();
    }
}
