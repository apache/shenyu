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


/**
 * DefaultConfigPropertySource .
 * <p>
 * 2019-08-15
 *
 * @author sixh
 */
public class DefaultConfigPropertySource implements ConfigPropertySource {

    private PropertyKeySource keySource;

    private PropertyKeyParse keyParse;

    public PropertyKeySource getKeySource() {
        return keySource;
    }

    public PropertyKeyParse getKeyParse() {
        return keyParse;
    }

    @Override
    public ConfigProperty findProperty(PropertyName propertyName) {
        if (propertyName.getElementSize() <= 0) {
            return null;
        }
        PropertyKey[] keys = getKeyParse().parse(propertyName);
        for (PropertyKey key : keys) {
            if (key.isAvailable(propertyName)) {
                ConfigProperty property = find(key);
                if (property != null) {
                    return property;
                }
            }
        }
        return null;
    }

    private ConfigProperty find(PropertyKey key) {
        String keyName = key.getKey();
        Object value = getKeySource().getValue(keyName);
        if (value == null) {
            return null;
        }
        PropertyName configurationPropertyName = key.getPropertyName();
        return ConfigProperty.of(configurationPropertyName, value);
    }
}
