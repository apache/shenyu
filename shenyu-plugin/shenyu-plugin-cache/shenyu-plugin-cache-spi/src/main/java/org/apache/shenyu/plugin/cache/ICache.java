/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.plugin.cache;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.MediaType;
import reactor.core.publisher.Mono;
import reactor.core.scheduler.Schedulers;

import java.nio.charset.StandardCharsets;

/**
 * ICache.
 */
public interface ICache {

    Logger LOG = LoggerFactory.getLogger(ICache.class);

    /**
     * Cache the data with the key.
     * @param key the cache key
     * @param bytes the data
     * @param timeoutSeconds value valid time
     * @return success or not
     */
    Mono<Boolean> cacheData(String key, byte[] bytes, long timeoutSeconds);

    /**
     * Check the cache is existed or not.
     * @param key the cache key
     * @return true exist
     */
    Mono<Boolean> isExist(String key);

    /**
     * Get data with the key.
     * @param key the cache key
     * @return the data
     */
    Mono<byte[]> getData(String key);

    /**
     * cache the content type.
     * @param key the key
     * @param mediaType the media type
     * @param timeoutSeconds value valid time
     * @return success or not
     */
    default void cacheContentType(final String key, final MediaType mediaType, final long timeoutSeconds) {
       cacheData(key, mediaTypeToBytes(mediaType), timeoutSeconds).subscribeOn(Schedulers.boundedElastic()).subscribe();
    }

    /**
     * Media type to bytes.
     * @param mediaType the media type
     * @return the media type bytes
     */
    default byte[] mediaTypeToBytes(final MediaType mediaType) {
        return mediaType.toString().getBytes(StandardCharsets.UTF_8);
    }

    /**
     * Get content type.
     * @param key the content type key
     * @return content type
     */
    default Mono<MediaType> getContentType(final String key) {
        return getData(key).map(v -> {
            if (v.length == 0) {
                return MediaType.APPLICATION_JSON;
            } else {
                return MediaType.valueOf(new String(v, StandardCharsets.UTF_8));
            }
        });

        // return MediaType.APPLICATION_JSON;


        // final byte[] data = getData(key);
        // if (Objects.isNull(data) || data.length == 0) {
        //     return MediaType.APPLICATION_JSON;
        // }
        // return MediaType.valueOf(new String(data, StandardCharsets.UTF_8));
    }

    /**
     * close the cache.
     */
    default void close() {

    }
}
