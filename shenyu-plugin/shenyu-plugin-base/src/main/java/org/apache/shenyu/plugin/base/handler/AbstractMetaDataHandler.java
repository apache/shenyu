package org.apache.shenyu.plugin.base.handler;

import java.net.URLClassLoader;

public abstract class AbstractMetaDataHandler implements MetaDataHandler {

    private URLClassLoader pluginClassLoader;

    @Override
    public void setPluginClassLoader(URLClassLoader classLoader) {
        this.pluginClassLoader = classLoader;
    }


    @Override
    public URLClassLoader getPluginClassLoader() {
        return this.pluginClassLoader;
    }
}
