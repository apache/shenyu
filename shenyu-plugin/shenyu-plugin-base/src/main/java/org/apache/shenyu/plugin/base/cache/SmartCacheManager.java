package org.apache.shenyu.plugin.base.cache;

import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.Optional;

/**
 * Smart cache manager with adaptive strategies.
 *
 * This interface provides a unified caching abstraction that can automatically
 * adapt between different caching strategies based on runtime characteristics.
 *
 * Key features:
 * 1. Adaptive strategy selection (LRU, Trie, Hybrid)
 * 2. Performance-based strategy switching
 * 3. Unified interface for different cache types
 * 4. Built-in metrics and monitoring
 */
public interface SmartCacheManager {

    /**
     * Get value from cache with adaptive strategy.
     *
     * @param key cache key
     * @param loader fallback loader if cache miss
     * @param <K> key type
     * @param <V> value type
     * @return cached or loaded value
     */
    <K, V> Mono<V> getOrLoad(String cacheType, K key, Mono<V> loader);

    /**
     * Put value into cache.
     *
     * @param cacheType cache type identifier
     * @param key cache key
     * @param value cache value
     * @param ttl time to live
     * @param <K> key type
     * @param <V> value type
     * @return completion signal
     */
    <K, V> Mono<Void> put(String cacheType, K key, V value, Duration ttl);

    /**
     * Invalidate cache entries.
     *
     * @param cacheType cache type identifier
     * @param key cache key
     * @param <K> key type
     * @return completion signal
     */
    <K> Mono<Void> invalidate(String cacheType, K key);

    /**
     * Clear all cache entries for a specific type.
     *
     * @param cacheType cache type identifier
     * @return completion signal
     */
    Mono<Void> clear(String cacheType);

    /**
     * Get cache statistics for monitoring.
     *
     * @param cacheType cache type identifier
     * @return cache statistics
     */
    CacheStats getStats(String cacheType);

    /**
     * Manually trigger strategy adaptation based on current performance.
     *
     * @param cacheType cache type identifier
     * @return completion signal
     */
    Mono<Void> adaptStrategy(String cacheType);

    /**
     * Cache type constants.
     */
    interface CacheTypes {
        String SELECTOR = "selector_cache";
        String RULE = "rule_cache";
        String PLUGIN = "plugin_cache";
        String UPSTREAM = "upstream_cache";
    }

    /**
     * Cache statistics for monitoring and adaptation.
     */
    class CacheStats {
        private final long hitCount;
        private final long missCount;
        private final long loadTime;
        private final long evictionCount;
        private final String strategy;
        private final double hitRatio;

        public CacheStats(long hitCount, long missCount, long loadTime,
                         long evictionCount, String strategy) {
            this.hitCount = hitCount;
            this.missCount = missCount;
            this.loadTime = loadTime;
            this.evictionCount = evictionCount;
            this.strategy = strategy;
            this.hitRatio = hitCount + missCount > 0 ?
                (double) hitCount / (hitCount + missCount) : 0.0;
        }

        // Getters
        public long getHitCount() { return hitCount; }
        public long getMissCount() { return missCount; }
        public long getLoadTime() { return loadTime; }
        public long getEvictionCount() { return evictionCount; }
        public String getStrategy() { return strategy; }
        public double getHitRatio() { return hitRatio; }

        @Override
        public String toString() {
            return String.format("CacheStats{hits=%d, misses=%d, hitRatio=%.2f%%, strategy=%s}",
                    hitCount, missCount, hitRatio * 100, strategy);
        }
    }
}