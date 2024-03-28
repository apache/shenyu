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

package org.apache.shenyu.springboot.starter.plugin.isolation.common;

import org.springframework.beans.factory.support.DefaultListableBeanFactory;

/**
 * The type Abstract isolation configuration.
 */
public abstract class AbstractIsolationConfiguration {

    /**
     * Register singleton.
     *
     * @param defaultListableBeanFactory the default listable bean factory
     * @param name                        the name
     * @param obj                         the obj
     */
    protected void registerSingleton(final DefaultListableBeanFactory defaultListableBeanFactory, final String name, final Object obj) {
        boolean existBean = defaultListableBeanFactory.containsBean(name);
        if (!existBean) {
            defaultListableBeanFactory.registerSingleton(name, obj);
        }
    }

    /**
     * Register singleton.
     *
     * @param defaultListableBeanFactory the default listable bean factory
     * @param obj                         the obj
     */
    protected void registerSingleton(final DefaultListableBeanFactory defaultListableBeanFactory, final Object obj) {
        String name = lowerCamelClassName(obj.getClass());
        boolean existBean = defaultListableBeanFactory.containsBean(name);
        if (!existBean) {
            defaultListableBeanFactory.registerSingleton(name, obj);
        }
    }

    /**
     * Lower camel class name string.
     *
     * @param clazz the clazz
     * @return the string
     */
    private String lowerCamelClassName(final Class<?> clazz) {
        String className = clazz.getSimpleName();
        if (Character.isLowerCase(className.charAt(0)) && !className.contains("_")) {
            return className;
        }
        String[] words = className.split("_");
        StringBuilder result = new StringBuilder(words[0].toLowerCase());

        for (int i = 1; i < words.length; i++) {
            result.append(capitalizeFirstLetter(words[i]));
        }
        return result.toString();
    }

    /**
     * Capitalize first letter string.
     *
     * @param word the word
     * @return the string
     */
    private String capitalizeFirstLetter(final String word) {
        return Character.toUpperCase(word.charAt(0)) + word.substring(1);
    }
}
