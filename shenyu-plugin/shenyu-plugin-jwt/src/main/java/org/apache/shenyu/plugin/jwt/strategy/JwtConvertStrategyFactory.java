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

package org.apache.shenyu.plugin.jwt.strategy;

import org.apache.shenyu.spi.ExtensionLoader;

import java.util.Objects;

public class JwtConvertStrategyFactory {

    /**
     * new instance jwtConvertStrategy.
     *
     * @param handleType handleType
     * @return jwtConvertStrategy
     */
    @SuppressWarnings("rawtypes")
    public static JwtConvertStrategy newInstance(final String handleType) {

        if (Objects.isNull(handleType)) {
            return new DefaultJwtConvertStrategy();
        }
        return ExtensionLoader.getExtensionLoader(JwtConvertStrategy.class)
                .getJoin(handleType);
    }
}
