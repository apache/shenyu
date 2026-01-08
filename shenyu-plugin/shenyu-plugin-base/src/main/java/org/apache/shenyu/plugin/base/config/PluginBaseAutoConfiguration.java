package org.apache.shenyu.plugin.base.config;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.plugin.base.cache.DefaultSmartCacheManager;
import org.apache.shenyu.plugin.base.cache.SmartCacheManager;
import org.apache.shenyu.plugin.base.metrics.MetricsHelper;
import org.apache.shenyu.plugin.base.metrics.NoOpMetricsHelper;
import org.apache.shenyu.plugin.base.route.DefaultRouteResolver;
import org.apache.shenyu.plugin.base.route.RouteResolver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Configuration for enhanced plugin base components.
 *
 * This configuration registers all required beans for the
 * enhanced plugin architecture.
 */
@Configuration
public class PluginBaseAutoConfiguration {

    private static final Logger LOG = LoggerFactory.getLogger(PluginBaseAutoConfiguration.class);

    /**
     * Create SmartCacheManager bean.
     *
     * @param shenyuConfig ShenYu configuration
     * @return SmartCacheManager instance
     */
    @Bean
    public SmartCacheManager smartCacheManager(ShenyuConfig shenyuConfig) {
        LOG.info("Initializing SmartCacheManager with enhanced caching");
        return new DefaultSmartCacheManager(shenyuConfig);
    }

    /**
     * Create RouteResolver bean.
     *
     * @param cacheManager SmartCacheManager instance
     * @return RouteResolver instance
     */
    @Bean
    public RouteResolver routeResolver(SmartCacheManager cacheManager) {
        LOG.info("Initializing DefaultRouteResolver with SmartCacheManager");
        return new DefaultRouteResolver(cacheManager);
    }

    /**
     * Create MetricsHelper bean (no-op implementation by default).
     *
     * Override this bean to provide custom metrics implementation.
     *
     * @return MetricsHelper instance
     */
    @Bean
    public MetricsHelper metricsHelper() {
        LOG.info("Initializing NoOpMetricsHelper (override to enable metrics)");
        return new NoOpMetricsHelper();
    }

    /**
     * Log cache configuration on startup.
     */
    @Bean
    public CacheConfigLogger cacheConfigLogger(ShenyuConfig shenyuConfig) {
        return new CacheConfigLogger(shenyuConfig);
    }

    /**
     * Logger for cache configuration details.
     */
    static class CacheConfigLogger {

        private static final Logger LOG = LoggerFactory.getLogger(CacheConfigLogger.class);

        public CacheConfigLogger(ShenyuConfig shenyuConfig) {
            logCacheConfiguration(shenyuConfig);
        }

        private void logCacheConfiguration(ShenyuConfig shenyuConfig) {
            LOG.info("=== Enhanced Plugin Base Configuration ===");

            if (shenyuConfig.getSelectorMatchCache() != null) {
                LOG.info("Selector Cache:");
                LOG.info("  - Initial Capacity: {}",
                        shenyuConfig.getSelectorMatchCache().getCache().getInitialCapacity());
                LOG.info("  - Maximum Size: {}",
                        shenyuConfig.getSelectorMatchCache().getCache().getMaximumSize());
            }

            if (shenyuConfig.getRuleMatchCache() != null) {
                LOG.info("Rule Cache:");
                LOG.info("  - Initial Capacity: {}",
                        shenyuConfig.getRuleMatchCache().getCache().getInitialCapacity());
                LOG.info("  - Maximum Size: {}",
                        shenyuConfig.getRuleMatchCache().getCache().getMaximumSize());
            }

            LOG.info("==========================================");
        }
    }
}
