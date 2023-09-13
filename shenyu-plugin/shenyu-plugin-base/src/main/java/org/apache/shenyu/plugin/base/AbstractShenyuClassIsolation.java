package org.apache.shenyu.plugin.base;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.isolation.Module;
import org.apache.shenyu.isolation.ModuleManager;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URLClassLoader;
import java.util.*;

public abstract class AbstractShenyuClassIsolation<T extends ShenyuPlugin> extends AbstractShenyuPlugin implements Module {

    protected static final Logger LOG = LoggerFactory.getLogger(AbstractShenyuClassIsolation.class);

    private URLClassLoader pluginClassLoader;

    public Mono<Void> isolationExecute(final ServerWebExchange exchange, final ShenyuPluginChain chain, final SelectorData selector, final RuleData rule) {
        if (Objects.isNull(pluginClassLoader)) {
            try {
                pluginClassLoader = ModuleManager.initClassLoader(new File(getPath()));
            } catch (MalformedURLException e) {
                LOG.error("init plugin classloader failed.");
                e.printStackTrace();
            }
        }

        ClassLoader current = Thread.currentThread().getContextClassLoader();
        try {
            Thread.currentThread().setContextClassLoader(pluginClassLoader);
            ServiceLoader<Module> loader = ServiceLoader.load(Module.class, pluginClassLoader);
            Iterator<Module> it = loader.iterator();
            T plugin = null;
            while (it.hasNext()) {
                T module = (T) it.next();
                // 注意：name()方法返回的值可能不匹配
                if (module.named().equals(name())) {
                    plugin = module;
                    break;
                }
            }

            if (plugin == null) {
                LOG.error("failed to find plugin: {}", plugin);
                return Mono.just(null);
            }
            return plugin.execute(exchange, chain);

        } catch (Throwable e) {
            e.printStackTrace();
        } finally {
            Thread.currentThread().setContextClassLoader(current);
        }
        return Mono.just(null);
    }
}
