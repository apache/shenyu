/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 */

package org.dromara.soul.config.api.property;

import lombok.Data;

import java.util.Optional;

/**
 * ConfigProperty .
 * <p>
 *
 * <p>
 * 2019-08-15 21:20
 *
 * @author chenbin sixh
 */
@Data
public class ConfigProperty {
    /**
     * 属性名秒.
     */
    private PropertyName name;

    /**
     * 属性值.
     */
    private Object value;

    public ConfigProperty(PropertyName name, Object value) {
        this.name = name;
        this.value = value;
    }

    public static ConfigProperty of(PropertyName name, Object value) {
        return Optional.ofNullable(value).map(cf -> new ConfigProperty(name, cf)).orElse(null);
    }
}
