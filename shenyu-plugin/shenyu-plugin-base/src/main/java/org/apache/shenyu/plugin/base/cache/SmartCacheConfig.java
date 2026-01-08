package org.apache.shenyu.plugin.base.cache;

import org.apache.shenyu.common.config.ShenyuConfig;

import java.time.Duration;
import java.util.HashMap;
import java.util.Map;

/**
 * Smart cache configuration with adaptive strategies.
 */
public class SmartCacheConfig {

    private CacheStrategy defaultStrategy = CacheStrategy.ADAPTIVE;
    private Map<String, CacheConfig> cacheConfigs = new HashMap<>();
    private AdaptiveConfig adaptiveConfig = new AdaptiveConfig();

    /**
     * Create configuration from existing ShenyuConfig.
     */
    public static SmartCacheConfig fromShenyuConfig(ShenyuConfig shenyuConfig) {
        SmartCacheConfig config = new SmartCacheConfig();

        // Extract selector cache config
        if (shenyuConfig.getSelectorMatchCache() != null) {
            CacheConfig selectorConfig = CacheConfig.fromLegacyConfig(
                shenyuConfig.getSelectorMatchCache(), CacheStrategy.ADAPTIVE);
            config.cacheConfigs.put("selector_cache", selectorConfig);
        }

        // Extract rule cache config
        if (shenyuConfig.getRuleMatchCache() != null) {
            CacheConfig ruleConfig = CacheConfig.fromLegacyRuleConfig(
                shenyuConfig.getRuleMatchCache(), CacheStrategy.ADAPTIVE);
            config.cacheConfigs.put("rule_cache", ruleConfig);
        }

        return config;
    }

    /**
     * Get cache configuration for specific type.
     */
    public CacheConfig getCacheConfig(String cacheType) {
        return cacheConfigs.getOrDefault(cacheType, createDefaultConfig());
    }

    private CacheConfig createDefaultConfig() {
        return CacheConfig.builder()
                .strategy(defaultStrategy)
                .initialCapacity(10000)
                .maximumSize(65536)
                .expireAfterWrite(Duration.ofMinutes(30))
                .expireAfterAccess(Duration.ofMinutes(10))
                .build();
    }

    // Getters and setters
    public CacheStrategy getDefaultStrategy() { return defaultStrategy; }
    public AdaptiveConfig getAdaptiveConfig() { return adaptiveConfig; }

    /**
     * Cache strategy enumeration.
     */
    public enum CacheStrategy {
        LRU_ONLY,    // Pure LRU cache
        TRIE_ONLY,   // Pure Trie cache for path matching
        HYBRID,      // L1 (LRU) + L2 (Trie)
        ADAPTIVE     // Automatically adapt based on performance
    }

    /**
     * Individual cache configuration.
     */
    public static class CacheConfig {
        private CacheStrategy strategy;
        private int initialCapacity;
        private long maximumSize;
        private Duration expireAfterWrite;
        private Duration expireAfterAccess;
        private AdaptiveConfig adaptiveConfig;

        public static CacheConfig fromLegacyConfig(
                ShenyuConfig.SelectorMatchCache legacyConfig,
                CacheStrategy defaultStrategy) {

            return CacheConfig.builder()
                    .strategy(defaultStrategy)
                    .initialCapacity(legacyConfig.getCache().getInitialCapacity())
                    .maximumSize(legacyConfig.getCache().getMaximumSize())
                    .expireAfterWrite(Duration.ofMinutes(30))
                    .expireAfterAccess(Duration.ofMinutes(10))
                    .build();
        }

        public static CacheConfig fromLegacyRuleConfig(
                ShenyuConfig.RuleMatchCache legacyConfig,
                CacheStrategy defaultStrategy) {

            return CacheConfig.builder()
                    .strategy(defaultStrategy)
                    .initialCapacity(legacyConfig.getCache().getInitialCapacity())
                    .maximumSize(legacyConfig.getCache().getMaximumSize())
                    .expireAfterWrite(Duration.ofMinutes(30))
                    .expireAfterAccess(Duration.ofMinutes(10))
                    .build();
        }

        public static Builder builder() {
            return new Builder();
        }

        // Builder pattern
        public static class Builder {
            private CacheStrategy strategy = CacheStrategy.ADAPTIVE;
            private int initialCapacity = 10000;
            private long maximumSize = 65536;
            private Duration expireAfterWrite = Duration.ofMinutes(30);
            private Duration expireAfterAccess = Duration.ofMinutes(10);

            public Builder strategy(CacheStrategy strategy) {
                this.strategy = strategy;
                return this;
            }

            public Builder initialCapacity(int initialCapacity) {
                this.initialCapacity = initialCapacity;
                return this;
            }

            public Builder maximumSize(long maximumSize) {
                this.maximumSize = maximumSize;
                return this;
            }

            public Builder expireAfterWrite(Duration expireAfterWrite) {
                this.expireAfterWrite = expireAfterWrite;
                return this;
            }

            public Builder expireAfterAccess(Duration expireAfterAccess) {
                this.expireAfterAccess = expireAfterAccess;
                return this;
            }

            public CacheConfig build() {
                CacheConfig config = new CacheConfig();
                config.strategy = this.strategy;
                config.initialCapacity = this.initialCapacity;
                config.maximumSize = this.maximumSize;
                config.expireAfterWrite = this.expireAfterWrite;
                config.expireAfterAccess = this.expireAfterAccess;
                config.adaptiveConfig = new AdaptiveConfig();
                return config;
            }
        }

        // Getters
        public CacheStrategy getStrategy() { return strategy; }
        public int getInitialCapacity() { return initialCapacity; }
        public long getMaximumSize() { return maximumSize; }
        public Duration getExpireAfterWrite() { return expireAfterWrite; }
        public Duration getExpireAfterAccess() { return expireAfterAccess; }
        public AdaptiveConfig getAdaptiveConfig() { return adaptiveConfig; }
    }

    /**
     * Configuration for adaptive caching behavior.
     */
    public static class AdaptiveConfig {
        private int switchThreshold = 1000;      // Switch to Trie after 1000 entries
        private double hitRatioThreshold = 0.8;  // Switch if hit ratio < 80%
        private Duration evaluationInterval = Duration.ofMinutes(5); // Evaluate every 5 minutes
        private boolean autoOptimization = true; // Enable automatic optimization

        // Getters and setters
        public int getSwitchThreshold() { return switchThreshold; }
        public void setSwitchThreshold(int switchThreshold) { this.switchThreshold = switchThreshold; }

        public double getHitRatioThreshold() { return hitRatioThreshold; }
        public void setHitRatioThreshold(double hitRatioThreshold) { this.hitRatioThreshold = hitRatioThreshold; }

        public Duration getEvaluationInterval() { return evaluationInterval; }
        public void setEvaluationInterval(Duration evaluationInterval) { this.evaluationInterval = evaluationInterval; }

        public boolean isAutoOptimization() { return autoOptimization; }
        public void setAutoOptimization(boolean autoOptimization) { this.autoOptimization = autoOptimization; }
    }
}