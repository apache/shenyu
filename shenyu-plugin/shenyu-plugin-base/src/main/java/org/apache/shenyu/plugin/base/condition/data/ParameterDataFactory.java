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

package org.apache.shenyu.plugin.base.condition.data;

import org.apache.shenyu.spi.ExtensionLoader;
import org.springframework.web.server.ServerWebExchange;

/**
 * The type Parameter data factory.
 */
public final class ParameterDataFactory {
    
    private ParameterDataFactory() {
    }
    
    /**
     * New instance parameter data.
     *
     * @param paramType the param type
     * @return the parameter data
     */
    public static ParameterData newInstance(final String paramType) {
        return ExtensionLoader.getExtensionLoader(ParameterData.class).getJoin(paramType);
    }
    
    /**
     * Builder data string.
     *
     * @param paramType the param type
     * @param paramName the param name
     * @param exchange the exchange
     * @return the string
     */
    public static String builderData(final String paramType, final String paramName, final ServerWebExchange exchange) {
        return newInstance(paramType).builder(paramName, exchange);
    }
}
