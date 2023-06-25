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

package org.apache.shenyu.web.endpoint;

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.TrieCacheTypeEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.apache.shenyu.plugin.base.cache.MetaDataCache;
import org.apache.shenyu.plugin.base.trie.ShenyuTrie;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Flux;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * The type Shenyu controller endpoint.
 */
@RestController
@RequestMapping(value = "/actuator", produces = {MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
public class ShenyuControllerEndpoint {
    
    private final ShenyuWebHandler webHandler;
    
    /**
     * Instantiates a new Shenyu controller endpoint.
     *
     * @param webHandler the web handler
     */
    public ShenyuControllerEndpoint(final ShenyuWebHandler webHandler) {
        this.webHandler = webHandler;
    }
    
    /**
     * Plugins flux.
     *
     * @return the flux
     */
    @GetMapping("/plugins")
    public Flux<Map<String, Integer>> plugins() {
        List<ShenyuPlugin> plugins = webHandler.getPlugins();
        return Flux.just(plugins.stream().collect(Collectors.toMap(ShenyuPlugin::toString, ShenyuPlugin::getOrder)));
    }
    
    /**
     * Plugin datas flux.
     *
     * @return the flux
     */
    @GetMapping("/pluginData")
    public Flux<Map<String, PluginData>> pluginDatas() {
        return Flux.just(BaseDataCache.getInstance().getPluginMap());
    }
    
    /**
     * Selector data flux.
     *
     * @return the flux
     */
    @GetMapping("/selectorData")
    public Flux<Map<String, List<SelectorData>>> selectorData() {
        return Flux.just(BaseDataCache.getInstance().getSelectorMap());
    }
    
    /**
     * Rule data flux.
     *
     * @return the flux
     */
    @GetMapping("/ruleData")
    public Flux<Map<String, List<RuleData>>> ruleData() {
        return Flux.just(BaseDataCache.getInstance().getRuleMap());
    }
    
    /**
     * selector match cache flux.
     *
     * @return the flux
     */
    @GetMapping("/selectorMatchCache")
    public Flux<Map<String, Map<String, SelectorData>>> getSelectorMatchCache() {
        return Flux.just(MatchDataCache.getInstance().getSelectorMatchCache());
    }
    
    /**
     * rule match cache flux.
     *
     * @return the flux
     */
    @GetMapping("/ruleMatchCache")
    public Flux<Map<String, Map<String, RuleData>>> getRuleMatchCache() {
        return Flux.just(MatchDataCache.getInstance().getRuleMatchCache());
    }
    
    /**
     * get metadata map.
     *
     * @return metadata map
     */
    @GetMapping("/metadata")
    public Flux<Map<String, MetaData>> getMetadata() {
        return Flux.just(MetaDataCache.getInstance().getMetaDataMap());
    }
    
    /**
     * get metadata cache.
     *
     * @return metadata cache
     */
    @GetMapping("/metadataCache")
    public Flux<Map<String, MetaData>> getMetaDataCache() {
        return Flux.just(MetaDataCache.getInstance().getMetaDataCache());
    }
    
    /**
     * get selector trie cache key set.
     *
     * @return trie cache key sets
     */
    @GetMapping("/selectorTrie")
    public Flux<Set<String>> getSelectorTrieKeys() {
        ShenyuTrie selectorTrie = SpringBeanUtils.getInstance().getBean(TrieCacheTypeEnum.SELECTOR.getTrieType());
        return Flux.just(selectorTrie.getKeyRootKeys());
    }
    
    /**
     * get rule trie cache key set.
     *
     * @return trie cache key sets
     */
    @GetMapping("/ruleTrie")
    public Flux<Set<String>> getRuleTrieKeys() {
        ShenyuTrie ruleTrie = SpringBeanUtils.getInstance().getBean(TrieCacheTypeEnum.RULE.getTrieType());
        return Flux.just(ruleTrie.getKeyRootKeys());
    }

}
