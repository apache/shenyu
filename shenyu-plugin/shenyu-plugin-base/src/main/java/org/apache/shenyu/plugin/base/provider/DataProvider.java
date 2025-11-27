package org.apache.shenyu.plugin.base.provider;

import java.util.List;

/**
 * A generic interface for providing data based on a given key.
 * <p>
 * Implementations of this interface are responsible for retrieving a list of data items
 * of type {@code T} associated with the specified key. This can be used in plugin systems
 * or other contexts where data needs to be fetched dynamically.
 *
 * @param <T> Names of various data types
 */
public interface DataProvider<T> {
    /**
     * Retrieves a list of data items associated with the specified key.
     *
     * @param key the key used to look up the data; its meaning is defined by the implementation
     * @return a list of data items of type {@code T} associated with the given key,
     *         or an empty list if no data is found
     */
    List<T> getData(String key);
}