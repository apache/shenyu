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

package org.apache.shenyu.plugin.cryptor.common.config;

import org.apache.shenyu.plugin.cryptor.common.annotation.Strategy;
import org.apache.shenyu.plugin.cryptor.common.strategies.CryptorStrategy;
import org.reflections.Reflections;

import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Cryptor register config.
 */
public class SubscribeConfig {

    private static final List<CryptorStrategy> STRATEGIES = new CopyOnWriteArrayList<>();

    static {
        String pageName = CryptorStrategy.class.getPackage().getName();
        Reflections reflections = new Reflections(pageName);
        Set<Class<?>> typesAnnotatedWith = reflections.getTypesAnnotatedWith(Strategy.class);
        Iterator<Class<?>> iterator = typesAnnotatedWith.iterator();
        try {
            while (iterator.hasNext()) {
                Object o = iterator.next().newInstance();
                if (o instanceof CryptorStrategy) {
                    STRATEGIES.add((CryptorStrategy) o);
                }
            }
        } catch (InstantiationException | IllegalAccessException e) {
            e.printStackTrace();
        }
    }

    /**
     * all strategy.
     * @return list.
     */
    public static List<CryptorStrategy> getStrategies() {
        return STRATEGIES;
    }

}
