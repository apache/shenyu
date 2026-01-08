package org.apache.shenyu.plugin.base.cache;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import reactor.core.publisher.Mono;

import java.time.Duration;

/**
 * Hybrid cache implementation combining LRU (L1) and Trie (L2) caching.
 *
 * This implementation provides two-level caching:
 * - L1: Fast LRU cache for frequently accessed items
 * - L2: Trie-based cache for path pattern matching
 *
 * TODO: Full implementation planned for Phase 2
 * Current status: L1 cache operational, L2 planned
 */
public class HybridCacheInstance<V> {

    private final SmartCacheConfig.CacheConfig config;
    private final Cache<Object, V> l1Cache; // Fast LRU cache
    private final TrieCacheInstance<V> l2Cache; // Trie-based cache

    public HybridCacheInstance(SmartCacheConfig.CacheConfig config) {
        this.config = config;

        // Initialize L1 cache (LRU)
        this.l1Cache = Caffeine.newBuilder()
                .initialCapacity(config.getInitialCapacity() / 10) // L1 is 10% of total
                .maximumSize(config.getMaximumSize() / 10)
                .expireAfterWrite(config.getExpireAfterWrite())
                .expireAfterAccess(config.getExpireAfterAccess())
                .recordStats()
                .build();

        // Initialize L2 cache (Trie)
        this.l2Cache = new TrieCacheInstance<>(config);
    }

    /**
     * Get value from cache with L1/L2 lookup.
     *
     * Lookup order:
     * 1. Check L1 cache (fast)
     * 2. Check L2 cache (slower but more intelligent)
     * 3. Load from source
     */
    public <K> Mono<V> getOrLoad(K key, Mono<V> loader) {
        // Try L1 cache first
        V l1Value = l1Cache.getIfPresent(key);
        if (l1Value != null) {
            return Mono.just(l1Value);
        }

        // Try L2 cache
        return l2Cache.getOrLoad(key, loader)
                .doOnNext(value -> {
                    if (value != null) {
                        // Promote to L1 cache on access
                        l1Cache.put(key, value);
                    }
                });
    }

    /**
     * Put value into both cache levels.
     */
    public <K> void put(K key, V value, Duration ttl) {
        l1Cache.put(key, value);
        l2Cache.put(key, value, ttl);
    }

    /**
     * Invalidate from both cache levels.
     */
    public <K> void invalidate(K key) {
        l1Cache.invalidate(key);
        l2Cache.invalidate(key);
    }

    /**
     * Clear both cache levels.
     */
    public void clear() {
        l1Cache.invalidateAll();
        l2Cache.clear();
    }

    /**
     * Get combined cache statistics.
     */
    public SmartCacheManager.CacheStats getStats() {
        com.github.benmanes.caffeine.cache.stats.CacheStats l1Stats = l1Cache.stats();
        SmartCacheManager.CacheStats l2Stats = l2Cache.getStats();

        // Combine statistics from both levels
        return new SmartCacheManager.CacheStats(
                l1Stats.hitCount() + l2Stats.getHitCount(),
                l1Stats.missCount() + l2Stats.getMissCount(),
                (long) l1Stats.averageLoadPenalty(),
                l1Stats.evictionCount() + l2Stats.getEvictionCount(),
                "HYBRID"
        );
    }
}
