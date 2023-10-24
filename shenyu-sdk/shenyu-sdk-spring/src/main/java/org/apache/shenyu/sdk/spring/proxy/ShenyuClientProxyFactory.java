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

package org.apache.shenyu.sdk.spring.proxy;

import org.apache.shenyu.sdk.spring.ShenyuClientFactoryBean;
import org.springframework.context.ApplicationContext;

import java.lang.reflect.Proxy;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * ShenyuClientProxyFactory.
 *
 */
public final class ShenyuClientProxyFactory {

    /**
     * PROXY_CACHE.
     */
    private static final ConcurrentMap<Class<?>, Object> PROXY_CACHE = new ConcurrentHashMap<>();

    /**
     * createProxy.
     *
     * @param apiClass apiClass
     * @param applicationContext applicationContext
     * @param shenyuClientFactoryBean shenyuClientFactoryBean
     * @return {@link Object}
     */
    public static Object createProxy(final Class<?> apiClass, final ApplicationContext applicationContext, final ShenyuClientFactoryBean shenyuClientFactoryBean) {
        if (!apiClass.isInterface()) {
            throw new UnsupportedOperationException("@ShenyuClient please use it on the interface. " + apiClass.getName());
        }

        if (PROXY_CACHE.containsKey(apiClass)) {
            return PROXY_CACHE.get(apiClass);
        }

        synchronized (apiClass) {
            Object proxy = Proxy.newProxyInstance(apiClass.getClassLoader(),
                    new Class<?>[]{apiClass},
                    new ShenyuClientInvocationHandler(apiClass, applicationContext, shenyuClientFactoryBean));
            PROXY_CACHE.put(apiClass, proxy);
        }
        return PROXY_CACHE.get(apiClass);
    }

}
