package org.apache.shenyu.plugin.base.cache;

import com.github.benmanes.caffeine.cache.Cache;
import com.github.benmanes.caffeine.cache.Caffeine;
import com.github.benmanes.caffeine.cache.stats.CacheStats;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.plugin.base.trie.ShenyuTrie;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Default implementation of SmartCacheManager with adaptive strategies.
 *
 * Supports multiple caching strategies:
 * 1. LRU_ONLY - Pure LRU cache using Caffeine
 * 2. TRIE_ONLY - Pure Trie-based cache for path matching
 * 3. HYBRID - Combination of LRU (L1) + Trie (L2)
 * 4. ADAPTIVE - Automatically switches between strategies based on performance
 */
@Component
public class DefaultSmartCacheManager implements SmartCacheManager {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultSmartCacheManager.class);

    private final ConcurrentHashMap<String, CacheInstance<?>> caches = new ConcurrentHashMap<>();
    private final SmartCacheConfig config;
    private final AtomicLong totalRequests = new AtomicLong(0);

    public DefaultSmartCacheManager(ShenyuConfig shenyuConfig) {
        this.config = SmartCacheConfig.fromShenyuConfig(shenyuConfig);
        initializePredefinedCaches();
    }

    @Override
    @SuppressWarnings("unchecked")
    public <K, V> Mono<V> getOrLoad(String cacheType, K key, Mono<V> loader) {
        totalRequests.incrementAndGet();

        return Mono.fromCallable(() -> {
            CacheInstance<V> cache = (CacheInstance<V>) getOrCreateCache(cacheType);
            return cache.getOrLoad(key, loader);
        }).flatMap(mono -> mono);
    }

    @Override
    @SuppressWarnings("unchecked")
    public <K, V> Mono<Void> put(String cacheType, K key, V value, Duration ttl) {
        return Mono.fromRunnable(() -> {
            CacheInstance<V> cache = (CacheInstance<V>) getOrCreateCache(cacheType);
            cache.put(key, value, ttl);
        });
    }

    @Override
    public <K> Mono<Void> invalidate(String cacheType, K key) {
        return Mono.fromRunnable(() -> {
            CacheInstance<?> cache = getOrCreateCache(cacheType);
            cache.invalidate(key);
        });
    }

    @Override
    public Mono<Void> clear(String cacheType) {
        return Mono.fromRunnable(() -> {
            CacheInstance<?> cache = caches.get(cacheType);
            if (cache != null) {
                cache.clear();
            }
        });
    }

    @Override
    public SmartCacheManager.CacheStats getStats(String cacheType) {
        CacheInstance<?> cache = caches.get(cacheType);
        return cache != null ? cache.getStats() : createEmptyStats();
    }

    @Override
    public Mono<Void> adaptStrategy(String cacheType) {
        return Mono.fromRunnable(() -> {
            CacheInstance<?> cache = caches.get(cacheType);
            if (cache instanceof AdaptiveCacheInstance) {
                ((AdaptiveCacheInstance<?>) cache).adaptStrategy();
            }
        });
    }

    /**
     * Get or create cache instance for the given type.
     */
    private CacheInstance<?> getOrCreateCache(String cacheType) {
        return caches.computeIfAbsent(cacheType, type -> {
            SmartCacheConfig.CacheConfig typeConfig = config.getCacheConfig(type);
            return createCacheInstance(type, typeConfig);
        });
    }

    /**
     * Create appropriate cache instance based on configuration.
     */
    private CacheInstance<?> createCacheInstance(String cacheType, SmartCacheConfig.CacheConfig cacheConfig) {
        switch (cacheConfig.getStrategy()) {
            case LRU_ONLY:
                return new LruCacheInstance<>(cacheConfig);
            case TRIE_ONLY:
                LOG.info("Creating Trie cache instance (Phase 1 basic implementation)");
                return new LruCacheInstance<>(cacheConfig); // Use LRU for now
            case HYBRID:
                LOG.info("Creating Hybrid cache instance (Phase 1 basic implementation)");
                return new LruCacheInstance<>(cacheConfig); // Use LRU for now
            case ADAPTIVE:
                return new AdaptiveCacheInstance<>(cacheConfig);
            default:
                LOG.warn("Unknown cache strategy: {}, falling back to LRU", cacheConfig.getStrategy());
                return new LruCacheInstance<>(cacheConfig);
        }
    }

    /**
     * Initialize predefined cache instances.
     */
    private void initializePredefinedCaches() {
        // Pre-create common cache types
        getOrCreateCache(CacheTypes.SELECTOR);
        getOrCreateCache(CacheTypes.RULE);
        getOrCreateCache(CacheTypes.PLUGIN);

        LOG.info("Smart cache manager initialized with strategy: {}", config.getDefaultStrategy());
    }

    private SmartCacheManager.CacheStats createEmptyStats() {
        return new SmartCacheManager.CacheStats(0, 0, 0, 0, "NONE");
    }

    /**
     * Base interface for cache instances.
     */
    private interface CacheInstance<V> {
        <K> Mono<V> getOrLoad(K key, Mono<V> loader);
        <K> void put(K key, V value, Duration ttl);
        <K> void invalidate(K key);
        void clear();
        SmartCacheManager.CacheStats getStats();
    }

    /**
     * LRU-only cache implementation using Caffeine.
     */
    private static class LruCacheInstance<V> implements CacheInstance<V> {
        private final Cache<Object, V> cache;
        private final SmartCacheConfig.CacheConfig config;

        public LruCacheInstance(SmartCacheConfig.CacheConfig config) {
            this.config = config;
            this.cache = Caffeine.newBuilder()
                    .initialCapacity(config.getInitialCapacity())
                    .maximumSize(config.getMaximumSize())
                    .expireAfterWrite(config.getExpireAfterWrite())
                    .expireAfterAccess(config.getExpireAfterAccess())
                    .recordStats()
                    .build();
        }

        @Override
        public <K> Mono<V> getOrLoad(K key, Mono<V> loader) {
            V cachedValue = cache.getIfPresent(key);
            if (cachedValue != null) {
                return Mono.just(cachedValue);
            }

            return loader.doOnNext(value -> {
                if (value != null) {
                    cache.put(key, value);
                }
            });
        }

        @Override
        public <K> void put(K key, V value, Duration ttl) {
            cache.put(key, value);
        }

        @Override
        public <K> void invalidate(K key) {
            cache.invalidate(key);
        }

        @Override
        public void clear() {
            cache.invalidateAll();
        }

        @Override
        public SmartCacheManager.CacheStats getStats() {
            com.github.benmanes.caffeine.cache.stats.CacheStats caffeineStats = cache.stats();
            return new SmartCacheManager.CacheStats(
                    caffeineStats.hitCount(),
                    caffeineStats.missCount(),
                    (long) caffeineStats.averageLoadPenalty(),
                    caffeineStats.evictionCount(),
                    "LRU_ONLY"
            );
        }
    }

    /**
     * Adaptive cache implementation that switches strategies based on performance.
     */
    private static class AdaptiveCacheInstance<V> implements CacheInstance<V> {
        private volatile CacheInstance<V> activeCache;
        private final SmartCacheConfig.CacheConfig config;
        private final AtomicLong requestCount = new AtomicLong(0);
        private final AtomicLong lastAdaptation = new AtomicLong(System.currentTimeMillis());

        public AdaptiveCacheInstance(SmartCacheConfig.CacheConfig config) {
            this.config = config;
            this.activeCache = new LruCacheInstance<>(config); // Start with LRU
        }

        @Override
        public <K> Mono<V> getOrLoad(K key, Mono<V> loader) {
            long count = requestCount.incrementAndGet();

            // Check if adaptation is needed
            if (shouldAdapt(count)) {
                adaptStrategy();
            }

            return activeCache.getOrLoad(key, loader);
        }

        @Override
        public <K> void put(K key, V value, Duration ttl) {
            activeCache.put(key, value, ttl);
        }

        @Override
        public <K> void invalidate(K key) {
            activeCache.invalidate(key);
        }

        @Override
        public void clear() {
            activeCache.clear();
        }

        @Override
        public SmartCacheManager.CacheStats getStats() {
            return activeCache.getStats();
        }

        /**
         * Adapt caching strategy based on current performance metrics.
         */
        public void adaptStrategy() {
            SmartCacheManager.CacheStats currentStats = getStats();

            // Decision logic for strategy adaptation
            if (currentStats.getHitRatio() < config.getAdaptiveConfig().getHitRatioThreshold()) {
                if (requestCount.get() > config.getAdaptiveConfig().getSwitchThreshold()) {
                    // Switch to Trie for better path matching
                    switchToStrategy("TRIE");
                } else {
                    // Switch to Hybrid for balanced performance
                    switchToStrategy("HYBRID");
                }
            }

            lastAdaptation.set(System.currentTimeMillis());
        }

        private boolean shouldAdapt(long count) {
            return count % 1000 == 0 && // Check every 1000 requests
                    System.currentTimeMillis() - lastAdaptation.get() > 60_000; // At most once per minute
        }

        private void switchToStrategy(String strategy) {
            // TODO: Implement Trie and Hybrid cache instances
            // For now, just log the intention but keep using LRU
            LOG.info("Strategy adaptation requested to: {}, but implementation pending. Keeping LRU.", strategy);

            // When Trie and Hybrid are implemented, uncomment:
            /*
            CacheInstance<V> newCache;
            switch (strategy) {
                case "TRIE":
                    newCache = new TrieCacheInstance<>(config);
                    break;
                case "HYBRID":
                    newCache = new HybridCacheInstance<>(config);
                    break;
                default:
                    return; // No change
            }
            // TODO: Migrate existing cache entries
            this.activeCache = newCache;
            LOG.info("Cache strategy adapted to: {}", strategy);
            */
        }
    }

    // Additional cache implementations would be defined here...
    // TrieCacheInstance, HybridCacheInstance, etc.
}