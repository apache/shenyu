/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.dromara.config.api;

import lombok.Data;

import java.util.List;

/**
 * PropertySource .
 * 属性数据源.
 *
 * @param <T> the type parameter
 * @author sixh
 */
@Data
public abstract class PropertySource<T> {

    /**
     * The Name.
     */
    protected final String name;

    /**
     * The Source.
     */
    protected final T source;

    /**
     * Instantiates a new Property source.
     *
     * @param name   the name
     * @param source the source
     */
    public PropertySource(String name, T source) {
        this.name = name;
        this.source = source;
    }

    /**
     * Gets property.
     *
     * @param propertySourceName the property source name
     * @return the property
     */
    public abstract Object getProperty(String propertySourceName);

    /**
     * 获取所有的keys.
     *
     * @return lsit.
     */
    public abstract List<String> getPropertyKeys();
}
