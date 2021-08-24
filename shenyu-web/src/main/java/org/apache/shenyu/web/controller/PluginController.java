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

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Mono;

/**
 * The type Plugin controller.
 */
@RestController
@RequestMapping("/shenyu")
public class PluginController {
    
    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(PluginController.class);
    
    private final PluginDataSubscriber subscriber;
    
    /**
     * Instantiates a new Plugin controller.
     *
     * @param subscriber the subscriber
     */
    public PluginController(final PluginDataSubscriber subscriber) {
        this.subscriber = subscriber;
    }
    
    /**
     * Clean all mono.
     *
     * @return the mono
     */
    @GetMapping("/cleanAll")
    public Mono<String> cleanAll() {
        subscriber.refreshPluginDataAll();
        subscriber.refreshSelectorDataAll();
        subscriber.refreshRuleDataAll();
        return Mono.just("success");
    }
    
    /**
     * Add plugin string.
     *
     * @param pluginData the plugin data
     * @return the string
     */
    @PostMapping("/plugin/saveOrUpdate")
    public Mono<String> saveOrUpdate(final PluginData pluginData) {
        LOG.info("saveOrUpdate apache shenyu local plugin");
        subscriber.onSubscribe(pluginData);
        return Mono.just("success");
    }
    
    /**
     * Delete mono.
     *
     * @param name the name
     * @return the mono
     */
    @GetMapping("/plugin/delete")
    public Mono<String> delete(@RequestParam("name") final String name) {
        LOG.info("delete apache shenyu local plugin");
        PluginData pluginData = PluginData.builder().name(name).build();
        subscriber.unSubscribe(pluginData);
        return Mono.just("success");
    }
    
    /**
     * Delete all mono.
     *
     * @return the mono
     */
    @GetMapping("/plugin/deleteAll")
    public Mono<String> deleteAll() {
        subscriber.refreshPluginDataAll();
        return Mono.just("success");
    }
    
    /**
     * Find by name mono.
     *
     * @param name the name
     * @return the mono
     */
    @GetMapping("/plugin/findByName")
    public Mono<PluginData> findByName(@RequestParam("name") final String name) {
        PluginData pluginData = BaseDataCache.getInstance().obtainPluginData(name);
        return Mono.just(pluginData);
    }
}
