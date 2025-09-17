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

package org.apache.shenyu.plugin.grpc.cache;

import com.google.common.collect.Maps;
import org.apache.shenyu.plugin.grpc.client.GrpcClientBuilder;
import org.apache.shenyu.plugin.grpc.client.ShenyuGrpcClient;

import java.util.Map;
import java.util.Objects;

/**
 * The Grpc client cache.
 */
public final class GrpcClientCache {
    
    private static final Map<String, ShenyuGrpcClient> CLIENT_CACHE = Maps.newConcurrentMap();
    
    static {
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            for (Map.Entry<String, ShenyuGrpcClient> entry : CLIENT_CACHE.entrySet()) {
                ShenyuGrpcClient grpcClient = entry.getValue();
                grpcClient.close();
            }
            CLIENT_CACHE.clear();
        }));
    }
    
    private GrpcClientCache() {
    }
    
    /**
     * Init client.
     *
     * @param selectorId selectorId
     */
    public static void initGrpcClient(final String selectorId) {
        CLIENT_CACHE.putIfAbsent(selectorId, GrpcClientBuilder.buildClient(selectorId));
    }
    
    /**
     * Get the client.
     *
     * @param selectorId selectorId
     * @return ShenyuGrpcClient shenyuGrpcClient
     */
    public static ShenyuGrpcClient getGrpcClient(final String selectorId) {
        return CLIENT_CACHE.get(selectorId);
    }
    
    /**
     * Remove client.
     *
     * @param selectorId selectorId
     */
    public static void removeClient(final String selectorId) {
        ShenyuGrpcClient grpcClient = CLIENT_CACHE.remove(selectorId);
        if (Objects.nonNull(grpcClient)) {
            grpcClient.close();
        }
    }
}
