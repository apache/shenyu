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

package org.apache.shenyu.examples.plugin.ext;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.reactive.DispatcherHandler;

/**
 * The type Ext plugin data handler.
 */
public class ExtPluginDataHandler implements PluginDataHandler {
    
    private static final Logger LOG = LoggerFactory.getLogger(ExtPluginDataHandler.class);
    
    private final DispatcherHandler dispatcherHandler;
    
    /**
     * Instantiates a new Ext plugin data handler.
     *
     * @param dispatcherHandler the dispatcher handler
     */
    public ExtPluginDataHandler(final DispatcherHandler dispatcherHandler) {
        this.dispatcherHandler = dispatcherHandler;
    }
    
    /**
     * Handler plugin.
     *
     * @param pluginData the plugin data
     */
    @Override
    public void handlerPlugin(final PluginData pluginData) {
        LOG.info("hello, im extend plugin dataHandler");
    }
    
    /**
     * Remove plugin.
     *
     * @param pluginData the plugin data
     */
    @Override
    public void removePlugin(final PluginData pluginData) {
        LOG.info("selector removed : name = {}", pluginData.getName());
    }
    
    /**
     * Handler selector.
     *
     * @param selectorData the selector data
     */
    @Override
    public void handlerSelector(final SelectorData selectorData) {
        LOG.info("selector processing : name = {}", selectorData.getName());
    }
    
    /**
     * Remove selector.
     *
     * @param selectorData the selector data
     */
    @Override
    public void removeSelector(final SelectorData selectorData) {
        LOG.info("selector removed : name = {}", selectorData.getName());
    }
    
    /**
     * Handler rule.
     *
     * @param ruleData the rule data
     */
    @Override
    public void handlerRule(final RuleData ruleData) {
        LOG.info("rule processing : name = {}", ruleData.getName());
    }
    
    /**
     * Remove rule.
     *
     * @param ruleData the rule data
     */
    @Override
    public void removeRule(final RuleData ruleData) {
        LOG.info("rule data removed: name = {}", ruleData.getName());
    }
    
    /**
     * Gets dispatcher handler.
     *
     * @return the dispatcher handler
     */
    public DispatcherHandler getDispatcherHandler() {
        return dispatcherHandler;
    }
    
    /**
     * Plugin named string.
     *
     * @return the string
     */
    @Override
    public String pluginNamed() {
        return "ext";
    }
}
