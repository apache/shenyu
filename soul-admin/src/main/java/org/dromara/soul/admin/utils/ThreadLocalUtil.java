package org.dromara.soul.admin.utils;

import java.util.HashMap;
import java.util.Map;

/**
 * The ThreadLocal utils.
 *
 * @author xiangminwen
 */
public class ThreadLocalUtil {
    private static final ThreadLocal<Map<String, Object>> THREAD_CONTEXT = new ThreadLocal<>();

    /**
     * save thread variable
     *
     * @param key   put key
     * @param value put value
     */
    public static void put(String key, Object value) {
        Map<String, Object> threadBidMap = THREAD_CONTEXT.get();
        if (threadBidMap == null) {
            threadBidMap = new HashMap<>();
            THREAD_CONTEXT.set(threadBidMap);
        }
        threadBidMap.put(key, value);
    }

    /**
     * remove thread variable
     *
     * @param key remove key
     */
    public static void remove(String key) {
        Map<String, Object> threadBidMap = THREAD_CONTEXT.get();
        if (threadBidMap != null) {
            threadBidMap.remove(key);
        }
    }

    /**
     * get thread variables
     *
     * @param key get key
     * @return the Object
     */
    public static Object get(String key) {
        Map<String, Object> threadBidMap = THREAD_CONTEXT.get();
        return threadBidMap != null ? threadBidMap.get(key) : null;
    }

    /**
     * remove all variables
     */
    public static void clear() {
        THREAD_CONTEXT.remove();
    }
}
