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

package org.apache.shenyu.web.controller;

import com.google.common.collect.Lists;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.RandomUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.apache.shenyu.common.enums.TrieCacheTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.apache.shenyu.plugin.base.trie.ShenyuTrie;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.stream.Collectors;

/**
 * The type Plugin controller.
 */
@RestController
@RequestMapping(value = "/shenyu", produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
public class LocalPluginController {

    private static final Logger LOG = LoggerFactory.getLogger(LocalPluginController.class);

    private final PluginDataSubscriber subscriber;

    private final DiscoveryUpstreamDataSubscriber discoveryUpstreamDataSubscriber;

    /**
     * Instantiates a new Plugin controller.
     *
     * @param subscriber the subscriber
     */
    public LocalPluginController(final PluginDataSubscriber subscriber,
                                 final DiscoveryUpstreamDataSubscriber discoveryUpstreamDataSubscriber) {
        this.subscriber = subscriber;
        this.discoveryUpstreamDataSubscriber = discoveryUpstreamDataSubscriber;
    }

    /**
     * Clean all mono.
     *
     * @return the mono
     */
    @GetMapping("/cleanAll")
    public Mono<String> cleanAll() {
        LOG.info("clean all apache shenyu local plugin");
        subscriber.refreshPluginDataAll();
        subscriber.refreshSelectorDataAll();
        subscriber.refreshRuleDataAll();
        return Mono.just(Constants.SUCCESS);
    }

    /**
     * Clean plugin mono.
     *
     * @param name the name
     * @return the mono
     */
    @GetMapping("/cleanPlugin")
    public Mono<String> cleanPlugin(@RequestParam("name") final String name) {
        LOG.info("clean apache shenyu local plugin for {}", name);
        BaseDataCache.getInstance().removePluginDataByPluginName(name);
        List<SelectorData> selectorData = Optional.ofNullable(BaseDataCache.getInstance().obtainSelectorData(name)).orElse(Collections.emptyList());
        final List<SelectorData> newSelectorData = CollectionUtils.isNotEmpty(selectorData) ? Lists.newArrayList(selectorData) : Collections.emptyList();
        final List<String> selectorIds = newSelectorData.stream().map(SelectorData::getId).collect(Collectors.toList());
        BaseDataCache.getInstance().removeSelectDataByPluginName(name);
        // remove selector and rule l1 cache
        MatchDataCache.getInstance().removeSelectorData(name);
        MatchDataCache.getInstance().removeRuleData(name);
        ShenyuTrie selectorTrie = SpringBeanUtils.getInstance().getBean(TrieCacheTypeEnum.SELECTOR.getTrieType());
        ShenyuTrie ruleTrie = SpringBeanUtils.getInstance().getBean(TrieCacheTypeEnum.RULE.getTrieType());
        ShenyuConfig shenyuConfig = SpringBeanUtils.getInstance().getBean(ShenyuConfig.class);
        // remove selector trie cache
        if (Boolean.TRUE.equals(shenyuConfig.getSelectorMatchCache().getTrie().getEnabled())) {
            newSelectorData.forEach(selector -> {
                List<ConditionData> conditionDataList = selector.getConditionList();
                if (CollectionUtils.isNotEmpty(conditionDataList)) {
                    List<ConditionData> filterConditions = conditionDataList.stream()
                            .filter(conditionData -> ParamTypeEnum.URI.getName().equals(conditionData.getParamType()))
                            .collect(Collectors.toList());
                    if (CollectionUtils.isNotEmpty(filterConditions)) {
                        List<String> uriPaths = filterConditions.stream().map(ConditionData::getParamValue).collect(Collectors.toList());
                        selectorTrie.remove(uriPaths, selector, TrieCacheTypeEnum.SELECTOR);
                    }
                }
            });
        }
        // remove rule trie cache
        for (String selectorId : selectorIds) {
            List<RuleData> ruleDataList = BaseDataCache.getInstance().obtainRuleData(selectorId);
            List<RuleData> newRuleDataList = CollectionUtils.isNotEmpty(ruleDataList) ? Lists.newArrayList(ruleDataList) : Collections.emptyList();
            BaseDataCache.getInstance().removeRuleDataBySelectorId(selectorId);
            if (Boolean.TRUE.equals(shenyuConfig.getRuleMatchCache().getTrie().getEnabled())) {
                if (CollectionUtils.isNotEmpty(newRuleDataList)) {
                    newRuleDataList.forEach(rule -> {
                        List<ConditionData> conditionDataList = rule.getConditionDataList();
                        if (CollectionUtils.isNotEmpty(conditionDataList)) {
                            List<ConditionData> filterConditions = conditionDataList.stream()
                                    .filter(conditionData -> ParamTypeEnum.URI.getName().equals(conditionData.getParamType()))
                                    .collect(Collectors.toList());
                            if (CollectionUtils.isNotEmpty(filterConditions)) {
                                List<String> uriPaths = filterConditions.stream().map(ConditionData::getParamValue).collect(Collectors.toList());
                                ruleTrie.remove(uriPaths, rule, TrieCacheTypeEnum.RULE);
                            }
                        }
                    });
                }
            }
        }
        return Mono.just(Constants.SUCCESS);
    }

    /**
     * Add plugin string.
     *
     * @param pluginData the plugin data
     * @return the string
     */
    @PostMapping("/plugin/saveOrUpdate")
    public Mono<String> saveOrUpdate(@RequestBody final PluginData pluginData) {
        LOG.info("saveOrUpdate apache shenyu local plugin for {}", pluginData.getName());
        subscriber.onSubscribe(pluginData);
        return Mono.just(Constants.SUCCESS);
    }

    /**
     * Delete mono.
     *
     * @param name the name
     * @return the mono
     */
    @GetMapping("/plugin/delete")
    public Mono<String> delete(@RequestParam("name") final String name) {
        LOG.info("delete apache shenyu local plugin for {}", name);
        PluginData pluginData = PluginData.builder().name(name).build();
        subscriber.unSubscribe(pluginData);
        return Mono.just(Constants.SUCCESS);
    }

    /**
     * Delete all mono.
     *
     * @return the mono
     */
    @GetMapping("/plugin/deleteAll")
    public Mono<String> deleteAll() {
        LOG.info("delete all apache shenyu local plugin");
        subscriber.refreshPluginDataAll();
        return Mono.just(Constants.SUCCESS);
    }

    /**
     * Find by name mono.
     *
     * @param name the name
     * @return the mono
     */
    @GetMapping("/plugin/findByName")
    public Mono<String> findByName(@RequestParam("name") final String name) {
        PluginData pluginData = BaseDataCache.getInstance().obtainPluginData(name);
        if (Objects.isNull(pluginData)) {
            return Mono.just("can not find this plugin : " + name);
        }
        return Mono.just(JsonUtils.toJson(pluginData));
    }

    /**
     * Save selector mono.
     *
     * @param selectorData the selector data
     * @return the mono
     */
    @PostMapping("/plugin/selector/saveOrUpdate")
    public Mono<String> saveSelector(@RequestBody final SelectorData selectorData) {
        if (StringUtils.isEmpty(selectorData.getPluginName())) {
            return Mono.just("Error: please add pluginName!");
        }
        SelectorData defaultSelectorData = buildDefaultSelectorData(selectorData);
        subscriber.onSelectorSubscribe(defaultSelectorData);
        saveDiscoveryUpstreamData(defaultSelectorData);
        return Mono.just(selectorData.getId());
    }

    private void saveDiscoveryUpstreamData(final SelectorData defaultSelectorData) {
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setSelectorId(defaultSelectorData.getId());
        discoverySyncData.setSelectorName(defaultSelectorData.getName());
        discoverySyncData.setPluginName(defaultSelectorData.getPluginName());
        List<DivideUpstream> upstreamList = GsonUtils.getInstance().fromList(defaultSelectorData.getHandle(), DivideUpstream.class);
        if (CollectionUtils.isNotEmpty(upstreamList)) {
            List<DiscoveryUpstreamData> discoveryUpstreamDataList = upstreamList.stream().map(up -> {
                DiscoveryUpstreamData upstreamData = new DiscoveryUpstreamData();
                upstreamData.setUrl(up.getUpstreamUrl());
                upstreamData.setProtocol(up.getProtocol());
                upstreamData.setWeight(up.getWeight());
                upstreamData.setStatus(up.isStatus() ? 0 : 1);
                Properties properties = new Properties();
                properties.setProperty("warmup", String.valueOf(up.getWarmup()));
                properties.setProperty("upstreamHost", String.valueOf(up.getUpstreamHost()));
                upstreamData.setDateUpdated(Optional.of(up.getTimestamp()).map(Timestamp::new).orElse(new Timestamp(System.currentTimeMillis())));
                upstreamData.setProps(GsonUtils.getInstance().toJson(properties));
                upstreamData.setDateCreated(Optional.of(up.getTimestamp()).map(Timestamp::new).orElse(new Timestamp(System.currentTimeMillis())));
                return upstreamData;
            }).collect(Collectors.toList());
            discoverySyncData.setUpstreamDataList(discoveryUpstreamDataList);
            discoveryUpstreamDataSubscriber.onSubscribe(discoverySyncData);
        }
    }

    /**
     * Selector and rule mono.
     *
     * @param selectorRuleData the selector rule data
     * @return the mono
     */
    @PostMapping("/plugin/selectorAndRule")
    public Mono<String> selectorAndRule(@RequestBody final SelectorRuleData selectorRuleData) {
        SelectorData selectorData = SelectorData.builder()
                .pluginName(selectorRuleData.getPluginName())
                .handle(selectorRuleData.getSelectorHandler())
                .conditionList(selectorRuleData.getConditionDataList())
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .build();
        SelectorData result = buildDefaultSelectorData(selectorData);
        subscriber.onSelectorSubscribe(result);
        saveDiscoveryUpstreamData(result);
        RuleData ruleData = RuleData.builder()
                .selectorId(result.getId())
                .matchRestful(Boolean.FALSE)
                .pluginName(selectorRuleData.getPluginName())
                .handle(selectorRuleData.getRuleHandler())
                .conditionDataList(selectorRuleData.getConditionDataList())
                .build();
        subscriber.onRuleSubscribe(buildDefaultRuleData(ruleData));
        return Mono.just(Constants.SUCCESS);
    }

    /**
     * Selector and rules mono.
     *
     * @param selectorRulesData the selector rules data
     * @return the mono
     */
    @PostMapping("/plugin/selectorAndRules")
    public Mono<String> selectorAndRules(@RequestBody final SelectorRulesData selectorRulesData) {
        SelectorData selectorData = SelectorData.builder()
                .pluginName(selectorRulesData.getPluginName())
                .handle(selectorRulesData.getSelectorHandler())
                .matchMode(selectorRulesData.getMatchMode())
                .conditionList(selectorRulesData.getConditionDataList())
                .type(SelectorTypeEnum.CUSTOM_FLOW.getCode())
                .sort(Optional.ofNullable(selectorRulesData.getSort()).orElse(10))
                .build();
        SelectorData result = buildDefaultSelectorData(selectorData);
        subscriber.onSelectorSubscribe(result);
        saveDiscoveryUpstreamData(result);
        List<RuleLocalData> ruleDataList = selectorRulesData.getRuleDataList();
        for (RuleLocalData data : ruleDataList) {
            RuleData ruleData = RuleData.builder()
                    .selectorId(result.getId())
                    .pluginName(result.getPluginName())
                    .name(data.getRuleName())
                    .matchMode(data.getMatchMode())
                    .handle(data.getRuleHandler())
                    .conditionDataList(data.getConditionDataList())
                    .build();
            subscriber.onRuleSubscribe(buildDefaultRuleData(ruleData));
        }
        return Mono.just(Constants.SUCCESS);
    }

    /**
     * Delete selector mono.
     *
     * @param pluginName the plugin name
     * @param id the id
     * @return the mono
     */
    @GetMapping("/plugin/selector/delete")
    public Mono<String> deleteSelector(@RequestParam("pluginName") final String pluginName,
                                       @RequestParam("id") final String id) {
        SelectorData selectorData = SelectorData.builder().pluginName(pluginName).id(id).build();
        subscriber.unSelectorSubscribe(selectorData);
        return Mono.just(Constants.SUCCESS);
    }

    /**
     * Find list selector mono.
     *
     * @param pluginName the plugin name
     * @param id the id
     * @return the mono
     */
    @GetMapping("/plugin/selector/findList")
    public Mono<String> findListSelector(@RequestParam("pluginName") final String pluginName,
                                         @RequestParam(value = "id", required = false) final String id) {
        List<SelectorData> selectorDataList = BaseDataCache.getInstance().obtainSelectorData(pluginName);
        if (CollectionUtils.isEmpty(selectorDataList)) {
            return Mono.just("Error: can not find selector data by pluginName :" + pluginName);
        }
        if (StringUtils.isNotEmpty(id)) {
            List<SelectorData> result = selectorDataList.stream().filter(selectorData -> selectorData.getId().equals(id)).collect(Collectors.toList());
            return Mono.just(JsonUtils.toJson(result));
        }
        return Mono.just(JsonUtils.toJson(selectorDataList));
    }

    /**
     * Save rule mono.
     *
     * @param ruleData the rule data
     * @return the mono
     */
    @PostMapping("/plugin/rule/saveOrUpdate")
    public Mono<String> saveRule(@RequestBody final RuleData ruleData) {
        if (StringUtils.isEmpty(ruleData.getSelectorId())) {
            return Mono.just("Error: please add selectorId!");
        }
        subscriber.onRuleSubscribe(buildDefaultRuleData(ruleData));
        return Mono.just(ruleData.getId());
    }

    /**
     * Delete rule mono.
     *
     * @param selectorId the selector id
     * @param id the id
     * @param pluginName the pluginName
     * @return the mono
     */
    @GetMapping("/plugin/rule/delete")
    public Mono<String> deleteRule(@RequestParam("selectorId") final String selectorId,
                                   @RequestParam("id") final String id,
                                   @RequestParam("pluginName") final String pluginName) {
        RuleData ruleData = RuleData.builder().selectorId(selectorId).id(id).pluginName(pluginName).build();
        subscriber.unRuleSubscribe(ruleData);
        return Mono.just(Constants.SUCCESS);
    }

    /**
     * Find list rule mono.
     *
     * @param selectorId the selector id
     * @param id the id
     * @return the mono
     */
    @GetMapping("/plugin/rule/findList")
    public Mono<String> findListRule(@RequestParam("selectorId") final String selectorId,
                                     @RequestParam(value = "id", required = false) final String id) {
        List<RuleData> ruleDataList = BaseDataCache.getInstance().obtainRuleData(selectorId);
        if (CollectionUtils.isEmpty(ruleDataList)) {
            return Mono.just("Error: can not find rule data by selector id :" + selectorId);
        }
        if (StringUtils.isNotEmpty(id)) {
            List<RuleData> result = ruleDataList.stream().filter(ruleData -> ruleData.getId().equals(id)).collect(Collectors.toList());
            return Mono.just(JsonUtils.toJson(result));
        }
        return Mono.just(JsonUtils.toJson(ruleDataList));
    }

    private SelectorData buildDefaultSelectorData(final SelectorData selectorData) {
        if (StringUtils.isEmpty(selectorData.getId())) {
            selectorData.setId(UUIDUtils.getInstance().generateShortUuid());
        }
        if (StringUtils.isEmpty(selectorData.getName())) {
            selectorData.setName(selectorData.getPluginName() + ":selector" + RandomUtils.nextInt(1, 1000));
        }
        if (Objects.isNull(selectorData.getMatchMode())) {
            selectorData.setMatchMode(MatchModeEnum.AND.getCode());
        }
        if (Objects.isNull(selectorData.getType())) {
            selectorData.setType(SelectorTypeEnum.FULL_FLOW.getCode());
        }
        if (Objects.isNull(selectorData.getSort())) {
            selectorData.setSort(10);
        }
        if (Objects.isNull(selectorData.getEnabled())) {
            selectorData.setEnabled(true);
        }
        if (Objects.isNull(selectorData.getLogged())) {
            selectorData.setLogged(false);
        }
        if (Objects.isNull(selectorData.getMatchRestful())) {
            selectorData.setMatchRestful(false);
        }
        return selectorData;
    }

    private RuleData buildDefaultRuleData(final RuleData ruleData) {
        if (StringUtils.isEmpty(ruleData.getId())) {
            ruleData.setId(UUIDUtils.getInstance().generateShortUuid());
        }
        if (StringUtils.isEmpty(ruleData.getName())) {
            ruleData.setName(ruleData.getPluginName() + ":rule" + RandomUtils.nextInt(1, 1000));
        }
        if (Objects.isNull(ruleData.getMatchMode())) {
            ruleData.setMatchMode(MatchModeEnum.AND.getCode());
        }
        if (Objects.isNull(ruleData.getSort())) {
            ruleData.setSort(10);
        }
        if (Objects.isNull(ruleData.getEnabled())) {
            ruleData.setEnabled(true);
        }
        if (Objects.isNull(ruleData.getLoged())) {
            ruleData.setLoged(false);
        }
        if (Objects.isNull(ruleData.getMatchRestful())) {
            ruleData.setMatchRestful(false);
        }
        return ruleData;
    }

    /**
     * The type Selector rule data.
     */
    public static class SelectorRuleData {

        private String pluginName;

        private String selectorName;

        private String selectorHandler;

        private String ruleHandler;

        private List<ConditionData> conditionDataList;

        /**
         * Gets plugin name.
         *
         * @return the plugin name
         */
        public String getPluginName() {
            return pluginName;
        }

        /**
         * Sets plugin name.
         *
         * @param pluginName the plugin name
         */
        public void setPluginName(final String pluginName) {
            this.pluginName = pluginName;
        }

        /**
         * Gets selector name.
         *
         * @return the selector name
         */
        public String getSelectorName() {
            return selectorName;
        }

        /**
         * Sets selector name.
         *
         * @param selectorName the selector name
         */
        public void setSelectorName(final String selectorName) {
            this.selectorName = selectorName;
        }

        /**
         * Gets selector handler.
         *
         * @return the selector handler
         */
        public String getSelectorHandler() {
            return selectorHandler;
        }

        /**
         * Sets selector handler.
         *
         * @param selectorHandler the selector handler
         */
        public void setSelectorHandler(final String selectorHandler) {
            this.selectorHandler = selectorHandler;
        }

        /**
         * Gets rule handler.
         *
         * @return the rule handler
         */
        public String getRuleHandler() {
            return ruleHandler;
        }

        /**
         * Sets rule handler.
         *
         * @param ruleHandler the rule handler
         */
        public void setRuleHandler(final String ruleHandler) {
            this.ruleHandler = ruleHandler;
        }

        /**
         * Gets condition data list.
         *
         * @return the condition data list
         */
        public List<ConditionData> getConditionDataList() {
            return conditionDataList;
        }

        /**
         * Sets condition data list.
         *
         * @param conditionDataList the condition data list
         */
        public void setConditionDataList(final List<ConditionData> conditionDataList) {
            this.conditionDataList = conditionDataList;
        }
    }

    /**
     * The type Selector rules data.
     */
    public static class SelectorRulesData {

        private String pluginName;

        private String selectorName;

        private Integer matchMode;

        private String selectorHandler;
        
        private Integer sort;

        private List<ConditionData> conditionDataList;

        private List<RuleLocalData> ruleDataList;

        /**
         * Gets plugin name.
         *
         * @return the plugin name
         */
        public String getPluginName() {
            return pluginName;
        }

        /**
         * Sets plugin name.
         *
         * @param pluginName the plugin name
         */
        public void setPluginName(final String pluginName) {
            this.pluginName = pluginName;
        }

        /**
         * Gets selector name.
         *
         * @return the selector name
         */
        public String getSelectorName() {
            return selectorName;
        }

        /**
         * Sets selector name.
         *
         * @param selectorName the selector name
         */
        public void setSelectorName(final String selectorName) {
            this.selectorName = selectorName;
        }

        /**
         * Gets selector handler.
         *
         * @return the selector handler
         */
        public String getSelectorHandler() {
            return selectorHandler;
        }

        /**
         * Sets selector handler.
         *
         * @param selectorHandler the selector handler
         */
        public void setSelectorHandler(final String selectorHandler) {
            this.selectorHandler = selectorHandler;
        }

        /**
         * Gets match mode.
         *
         * @return the match mode
         */
        public Integer getMatchMode() {
            return matchMode;
        }

        /**
         * Sets match mode.
         *
         * @param matchMode the match mode
         */
        public void setMatchMode(final Integer matchMode) {
            this.matchMode = matchMode;
        }
        
        /**
         * Gets sort.
         *
         * @return the sort
         */
        public Integer getSort() {
            return sort;
        }
        
        /**
         * Sets sort.
         *
         * @param sort the sort
         */
        public void setSort(final Integer sort) {
            this.sort = sort;
        }

        /**
         * Gets condition data list.
         *
         * @return the condition data list
         */
        public List<ConditionData> getConditionDataList() {
            return conditionDataList;
        }

        /**
         * Sets condition data list.
         *
         * @param conditionDataList the condition data list
         */
        public void setConditionDataList(final List<ConditionData> conditionDataList) {
            this.conditionDataList = conditionDataList;
        }

        /**
         * Gets rule data list.
         *
         * @return the rule data list
         */
        public List<RuleLocalData> getRuleDataList() {
            return ruleDataList;
        }

        /**
         * Sets rule data list.
         *
         * @param ruleDataList the rule data list
         */
        public void setRuleDataList(final List<RuleLocalData> ruleDataList) {
            this.ruleDataList = ruleDataList;
        }

    }

    /**
     * The type Rule data dto.
     */
    public static class RuleLocalData {

        private String ruleName;

        private String ruleHandler;

        private Integer matchMode;

        private List<ConditionData> conditionDataList;

        /**
         * Gets rule name.
         *
         * @return the rule name
         */
        public String getRuleName() {
            return ruleName;
        }

        /**
         * Sets rule name.
         *
         * @param ruleName the rule name
         */
        public void setRuleName(final String ruleName) {
            this.ruleName = ruleName;
        }

        /**
         * Gets rule handler.
         *
         * @return the rule handler
         */
        public String getRuleHandler() {
            return ruleHandler;
        }

        /**
         * Sets rule handler.
         *
         * @param ruleHandler the rule handler
         */
        public void setRuleHandler(final String ruleHandler) {
            this.ruleHandler = ruleHandler;
        }

        /**
         * Gets match mode.
         *
         * @return the match mode
         */
        public Integer getMatchMode() {
            return matchMode;
        }

        /**
         * Sets match mode.
         *
         * @param matchMode the match mode
         */
        public void setMatchMode(final Integer matchMode) {
            this.matchMode = matchMode;
        }

        /**
         * Gets condition data list.
         *
         * @return the condition data list
         */
        public List<ConditionData> getConditionDataList() {
            return conditionDataList;
        }

        /**
         * Sets condition data list.
         *
         * @param conditionDataList the condition data list
         */
        public void setConditionDataList(final List<ConditionData> conditionDataList) {
            this.conditionDataList = conditionDataList;
        }
    }
}
