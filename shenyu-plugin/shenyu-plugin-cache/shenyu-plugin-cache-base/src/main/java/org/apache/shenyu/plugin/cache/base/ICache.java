package org.apache.shenyu.plugin.cache.base;

/**
 * ICache.
 */
public interface ICache {

    /**
     * Cache the data with the key.
     * @param key the cache key
     * @param bytes the data
     * @param timeoutSeconds the timeout seconds
     * @return success or not
     */
    boolean cache(final String key, final byte[] bytes, final long timeoutSeconds);

    /**
     * Check the cache is exist or not.
     * @param key the cache key
     * @return true exist
     */
    boolean isExist(final String key);

    /**
     * Get data with the key.
     * @param key the cache key
     * @return the data
     */
    byte[] getData(final String key);
}
