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

package org.dromara.config.api.source;

import lombok.Data;

/**
 * PropertyKey .
 *
 * @author sixh
 */
@Data
public class PropertyKey {

    private String propertyName;

    private PropertyName propertyNameObj;

    /**
     * Instantiates a new Property key.
     *
     * @param propertyName    the property name
     * @param propertyNameObj the property name obj
     */
    public PropertyKey(String propertyName, PropertyName propertyNameObj) {
        this.propertyName = propertyName;
        this.propertyNameObj = propertyNameObj;
    }

    /**
     * Return if this mapping is applicable for the given
     * {@link ConfigurationPropertyName}.
     *
     * @param name the name to check
     * @return if the mapping is applicable
     */
    public boolean isApplicable(PropertyName name) {
        return this.propertyNameObj.equals(name);
    }
}
