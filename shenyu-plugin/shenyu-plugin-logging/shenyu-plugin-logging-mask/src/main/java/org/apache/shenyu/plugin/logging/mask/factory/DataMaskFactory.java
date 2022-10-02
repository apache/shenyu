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

package org.apache.shenyu.plugin.logging.mask.factory;

import org.apache.shenyu.plugin.logging.mask.spi.ShenyuDataMask;
import org.apache.shenyu.spi.ExtensionLoader;

/**
 * shenyu logging mask factory.
 */
public final class DataMaskFactory {

    public DataMaskFactory() {
    }

    /**
     * shenyu logging mask algorithm selector.
     *
     * @param source source data
     * @param algorithm mask algorithm
     * @return masked data
     */
    public static String selectMask(final String source, final String algorithm) {
        ShenyuDataMask dataMask = ExtensionLoader.getExtensionLoader(ShenyuDataMask.class).getJoin(algorithm);
        return dataMask.mask(source);
    }
}
