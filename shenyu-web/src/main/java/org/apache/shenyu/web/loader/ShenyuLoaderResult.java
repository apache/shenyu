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

package org.apache.shenyu.web.loader;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.context.ShenyuContextDecorator;
import org.apache.shenyu.plugin.base.handler.MetaDataHandler;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;

/**
 * The type of ShenYu Loader result.
 */
public class ShenyuLoaderResult {

    private String className;
    
    private ShenyuPlugin shenyuPlugin;
    
    private PluginDataHandler pluginDataHandler;

    private MetaDataHandler metaDataHandler;

    private ShenyuContextDecorator shenyuContextDecorator;

    public String getClassName() {
        return className;
    }

    public void setClassName(String className) {
        this.className = className;
    }

    /**
     * Gets shenyu plugin.
     *
     * @return the shenyu plugin
     */
    public ShenyuPlugin getShenyuPlugin() {
        return shenyuPlugin;
    }
    
    /**
     * Sets shenyu plugin.
     *
     * @param shenyuPlugin the shenyu plugin
     */
    public void setShenyuPlugin(final ShenyuPlugin shenyuPlugin) {
        this.shenyuPlugin = shenyuPlugin;
    }
    
    /**
     * Gets plugin data handler.
     *
     * @return the plugin data handler
     */
    public PluginDataHandler getPluginDataHandler() {
        return pluginDataHandler;
    }
    
    /**
     * Sets plugin data handler.
     *
     * @param pluginDataHandler the plugin data handler
     */
    public void setPluginDataHandler(final PluginDataHandler pluginDataHandler) {
        this.pluginDataHandler = pluginDataHandler;
    }

    /**
     * metaDataHandler.
     *
     * @return MetaDataHandler
     */
    public MetaDataHandler getMetaDataHandler() {
        return metaDataHandler;
    }

    /**
     * set metaDataHandler.
     *
     * @param metaDataHandler metaDataHandler
     */
    public void setMetaDataHandler(final MetaDataHandler metaDataHandler) {
        this.metaDataHandler = metaDataHandler;
    }

    /**
     * shenyuContextDecorator.
     *
     * @return ShenyuContextDecorator
     */
    public ShenyuContextDecorator getShenyuContextDecorator() {
        return shenyuContextDecorator;
    }

    /**
     * set shenyuContextDecorator.
     *
     * @param shenyuContextDecorator shenyuContextDecorator
     */
    public void setShenyuContextDecorator(final ShenyuContextDecorator shenyuContextDecorator) {
        this.shenyuContextDecorator = shenyuContextDecorator;
    }
}
