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

package org.apache.shenyu.plugin.context.path;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.ContextMappingRuleHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.base.AbstractShenyuPlugin;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.context.path.handler.ContextPathPluginDataHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * ContextPath Plugin.
 */
public class ContextPathPlugin extends AbstractShenyuPlugin {
    
    private static final Logger LOG = LoggerFactory.getLogger(ContextPathPlugin.class);
    
    private final ContextMappingRuleHandle defaultRuleHandle;
    
    {
        defaultRuleHandle = new ContextMappingRuleHandle();
        defaultRuleHandle.setAddPrefixed(true);
        defaultRuleHandle.setContextPath("/default");
    }
    
    @Override
    protected Mono<Void> doExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        ShenyuContext shenyuContext = exchange.getAttribute(Constants.CONTEXT);
        assert shenyuContext != null;
        ContextMappingRuleHandle ruleHandle = buildRuleHandle(rule);
        if (Objects.isNull(ruleHandle)) {
            LOG.error("context path rule configuration is null ：{}", rule);
            return chain.execute(exchange);
        }
        buildRealURI(shenyuContext, ruleHandle);
        return chain.execute(exchange);
    }
    
    @Override
    public int getOrder() {
        return PluginEnum.CONTEXT_PATH.getCode();
    }
    
    @Override
    public String named() {
        return PluginEnum.CONTEXT_PATH.getName();
    }
    
    @Override
    public boolean skip(final ServerWebExchange exchange) {
        return skip(exchange,
                RpcTypeEnum.DUBBO,
                RpcTypeEnum.GRPC,
                RpcTypeEnum.TARS,
                RpcTypeEnum.MOTAN,
                RpcTypeEnum.SOFA,
                RpcTypeEnum.BRPC);
    }
    
    private ContextMappingRuleHandle buildRuleHandle(final RuleData rule) {
        if (StringUtils.isNotEmpty(rule.getId())) {
            return ContextPathPluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        } else {
            return defaultRuleHandle;
        }
    }
    
    /**
     * Build the realUrl.
     *
     * @param context context
     * @param handle  handle
     */
    private void buildRealURI(final ShenyuContext context, final ContextMappingRuleHandle handle) {
        String realURI = "";
        String contextPath = handle.getContextPath();
        if (StringUtils.isNoneBlank(contextPath)) {
            context.setContextPath(contextPath);
            context.setModule(contextPath);
            if (handle.getAddPrefixed()) {
                realURI = context.getPath();
            } else {
                realURI = context.getPath().substring(contextPath.length());
            }
        }
        String addPrefix = handle.getAddPrefix();
        if (StringUtils.isNoneBlank(addPrefix)) {
            if (StringUtils.isNotBlank(realURI)) {
                realURI = addPrefix + realURI;
            } else {
                realURI = addPrefix + context.getPath();
            }
        }
        context.setRealUrl(realURI);
    }
}
