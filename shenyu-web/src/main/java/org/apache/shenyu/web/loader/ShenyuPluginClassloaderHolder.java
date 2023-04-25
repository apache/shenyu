package org.apache.shenyu.web.loader;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class ShenyuPluginClassloaderHolder {
    private ShenyuPluginClassloaderHolder() {
    }

    private final static ShenyuPluginClassloaderHolder singleton = new ShenyuPluginClassloaderHolder();


    public static ShenyuPluginClassloaderHolder getSingleton() {
        return singleton;
    }

    private Map<String, ShenyuPluginLoader> cache = new ConcurrentHashMap<>();

    public ShenyuPluginLoader get(String pluginName) {
        return cache.computeIfAbsent(pluginName, key -> {
            return new ShenyuPluginLoader();
        });
    }

    public void remove(String pluginName) {
        ShenyuPluginLoader removedLoader = cache.remove(pluginName);
        removedLoader.close();
    }


}
