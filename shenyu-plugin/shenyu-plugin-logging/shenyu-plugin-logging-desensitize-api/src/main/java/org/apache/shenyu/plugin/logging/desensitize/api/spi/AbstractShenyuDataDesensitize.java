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

package org.apache.shenyu.plugin.logging.desensitize.api.spi;

import org.apache.commons.lang3.StringUtils;

/**
 * AbstractShenyuDataDesensitize.
 */
public abstract class AbstractShenyuDataDesensitize implements ShenyuDataDesensitize {
    @Override
    public String desensitize(final String source) {
        if (StringUtils.isBlank(source)) {
            return "";
        }
        return doDesensitize(source);
    }

    /**
     * do desensitized data.
     *
     * @param source source
     * @return desensitized data
     */
    protected abstract String doDesensitize(String source);

}
