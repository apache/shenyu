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

package org.apache.shenyu.plugin.cache.utils;

import org.apache.commons.codec.digest.DigestUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.cache.ICache;
import org.springframework.web.server.ServerWebExchange;

import java.net.URI;

/**
 * CacheUtils.
 */
public final class CacheUtils {

    private static final String CONTENT_TYPEKEY_SUFFIX = "contentType";

    private static final String KEY_JOIN_RULE = "-";

    private CacheUtils() {
    }

    /**
     * the cache data key.
     *
     * @param exchange the exchange.
     * @return data key
     */
    public static String dataKey(final ServerWebExchange exchange) {
        //// todo 2022/3/16 current use the request path, maybe use the key from admin config.
        URI uri = exchange.getRequest().getURI();
        return DigestUtils.md5Hex(String.join(KEY_JOIN_RULE, uri.getQuery(), uri.getPath()));
    }

    /**
     * the cache data content type key.
     *
     * @param exchange the exchange
     * @return the content type key
     */
    public static String contentTypeKey(final ServerWebExchange exchange) {
        return String.join(KEY_JOIN_RULE, dataKey(exchange), CONTENT_TYPEKEY_SUFFIX);
    }

    /**
     * get the cache.
     *
     * @return cache
     */
    public static ICache getCache() {
        return Singleton.INST.get(ICache.class);
    }
}
