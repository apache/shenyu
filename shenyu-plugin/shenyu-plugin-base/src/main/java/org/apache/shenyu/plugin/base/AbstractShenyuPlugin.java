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

package org.apache.shenyu.plugin.base;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.maker.PluginDataDecisionMaker;
import org.apache.shenyu.plugin.base.maker.RuleDataDecisionMaker;
import org.apache.shenyu.plugin.base.maker.SelectorDataDecisionMaker;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import java.util.List;
import java.util.Objects;

/**
 * abstract shenyu plugin please extends.
 */
public abstract class AbstractShenyuPlugin implements ShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractShenyuPlugin.class);

    private SelectorDataDecisionMaker selectorDataDecisionMaker = new SelectorDataDecisionMaker();

    private RuleDataDecisionMaker ruleDataDecisionMaker = new RuleDataDecisionMaker();

    private PluginDataDecisionMaker pluginDataDecisionMaker = new PluginDataDecisionMaker();

    /**
     * this is Template Method child has implements your own logic.
     *
     * @param exchange exchange the current server exchange {@linkplain ServerWebExchange}
     * @param chain chain the current chain  {@linkplain ServerWebExchange}
     * @param selector selector    {@linkplain SelectorData}
     * @param rule rule    {@linkplain RuleData}
     * @return {@code Mono<Void>} to indicate when request handling is complete
     */
    protected abstract Mono<Void> doExecute(ServerWebExchange exchange, ShenyuPluginChain chain, SelectorData selector, RuleData rule);

    /**
     * Process the Web request and (optionally) delegate to the next
     * {@code ShenyuPlugin} through the given {@link ShenyuPluginChain}.
     *
     * @param exchange the current server exchange
     * @param chain    provides a way to delegate to the next plugin
     * @return {@code Mono<Void>} to indicate when request processing is complete
     */
    @Override
    public Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        final String pluginName = named();
        final String path = getRawPath(exchange);

        List<PluginData> pluginDataList = pluginDataDecisionMaker.getData(pluginName);
        if (CollectionUtils.isEmpty(pluginDataList) || !pluginDataDecisionMaker.shouldContinue(pluginDataList.get(0))) {
            return pluginDataDecisionMaker.handleEmpty(pluginName, exchange, chain);
        }

        List<SelectorData> selectorDataList = selectorDataDecisionMaker.getData(pluginName);
        if (CollectionUtils.isEmpty(selectorDataList)) {
            return selectorDataDecisionMaker.handleEmpty(pluginName, exchange, chain);
        }

        SelectorData selectorData = selectorDataDecisionMaker.matchData(exchange, pluginName, selectorDataList, path, null);
        if (Objects.isNull(selectorData)) {
            return selectorDataDecisionMaker.handleEmpty(pluginName, exchange, chain);
        }

        printLog(selectorData, pluginName);
        if (!selectorDataDecisionMaker.shouldContinue(selectorData)) {
            return doExecute(exchange, chain, selectorData, defaultRuleData(selectorData));
        }

        List<RuleData> ruleDataList = ruleDataDecisionMaker.getData(selectorData.getId());
        if (CollectionUtils.isEmpty(ruleDataList)) {
            return ruleDataDecisionMaker.handleEmpty(pluginName, exchange, chain);
        }

        if (selectorData.getType() == SelectorTypeEnum.FULL_FLOW.getCode()) {
            RuleData rule = ruleDataList.get(ruleDataList.size() - 1);
            printLog(rule, pluginName);
            return doExecute(exchange, chain, selectorData, rule);
        }

        RuleData ruleData = ruleDataDecisionMaker.matchData(exchange, named(), ruleDataList, path, selectorData);
        if (Objects.isNull(ruleData)) {
            return ruleDataDecisionMaker.handleEmpty(pluginName, exchange, chain);
        }

        printLog(ruleData, pluginName);
        return doExecute(exchange, chain, selectorData, ruleData);
    }

    protected String getRawPath(final ServerWebExchange exchange) {
        return exchange.getRequest().getURI().getRawPath();
    }


    private RuleData defaultRuleData(final SelectorData selectorData) {
        RuleData ruleData = new RuleData();
        ruleData.setSelectorId(selectorData.getId());
        ruleData.setPluginName(selectorData.getPluginName());
        ruleData.setId(Constants.DEFAULT_RULE);
        return ruleData;
    }

    /**
     * Handle selector if null mono.
     *
     * @param pluginName the plugin name
     * @param exchange the exchange
     * @param chain the chain
     * @return the mono
     */
    protected Mono<Void> handleSelectorIfNull(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return chain.execute(exchange);
    }

    /**
     * Handle rule if null mono.
     *
     * @param pluginName the plugin name
     * @param exchange the exchange
     * @param chain the chain
     * @return the mono
     */
    protected Mono<Void> handleRuleIfNull(final String pluginName, final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        return chain.execute(exchange);
    }

    /**
     * print selector log.
     * please don't delete this method or refactor {@linkplain org.apache.shenyu.plugin.base.AbstractShenyuPlugin#printLog}
     * because instanceof and class cast waste 10% cpu.
     *
     * @param selectorData selector data
     * @param pluginName plugin name
     */
    private void printLog(final SelectorData selectorData, final String pluginName) {
        if (selectorData.getLogged()) {
            LOG.info("{} selector success match , selector name :{}", pluginName, selectorData.getName());
        }
    }

    /**
     * print rule log.
     *
     * @param ruleData rule data
     * @param pluginName plugin name
     */
    private void printLog(final RuleData ruleData, final String pluginName) {
        if (ruleData.getLoged()) {
            LOG.info("{} rule success match , rule name :{}", pluginName, ruleData.getName());
        }
    }

}
