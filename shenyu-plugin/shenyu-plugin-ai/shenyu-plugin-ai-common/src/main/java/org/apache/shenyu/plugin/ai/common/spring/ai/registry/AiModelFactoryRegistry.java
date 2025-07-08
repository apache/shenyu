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

package org.apache.shenyu.plugin.ai.common.spring.ai.registry;

import org.apache.shenyu.common.enums.AiModelProviderEnum;
import org.apache.shenyu.plugin.ai.common.spring.ai.AiModelFactory;

import java.util.List;

/**
 * this is the AiModelFactoryRegistry.
 */
public class AiModelFactoryRegistry {

    private final List<AiModelFactory> factories;

    public AiModelFactoryRegistry(final List<AiModelFactory> factories) {
        this.factories = factories;
    }

    /**
     * Get ai model factory .
     *
     * @param modelType modelType
     * @return aiModelFactory
     */
    public AiModelFactory getFactory(final AiModelProviderEnum modelType) {
        return factories.stream()
                .filter(f -> f.supports(modelType))
                .findFirst()
                .orElseThrow(() -> new IllegalArgumentException("Unsupported AI model: " + modelType));
    }

}
