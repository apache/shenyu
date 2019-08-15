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

package org.dromara.config.core.property;

import lombok.Data;

/**
 * PropertyKey .
 * <p>
 * 2019-08-15
 *
 * @author sixh
 */
@Data
public class PropertyKey {

    private String key;

    private PropertyName propertyName;

    public PropertyKey(String key, PropertyName propertyName) {
        this.key = key;
        this.propertyName = propertyName;
    }

    /**
     * 是适用的.
     *
     * @param name eq {@link PropertyName#equals}
     * @return true else false.
     */
    public boolean isAvailable(PropertyName name) {
        return propertyName.equals(name);
    }
}


