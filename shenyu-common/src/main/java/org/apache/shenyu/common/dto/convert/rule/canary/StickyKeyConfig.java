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

package org.apache.shenyu.common.dto.convert.rule.canary;

import java.util.Objects;

/**
 * Identifier source for stable canary grouping.
 */
public class StickyKeyConfig {

    /**
     * Registered ParameterData SPI source name, such as header, cookie, query, ip or a custom extension.
     */
    private String paramType;

    /**
     * Source-specific parameter name, passed unchanged to the selected ParameterData implementation.
     * Canary does not validate the parameter name; the source determines how it is used.
     */
    private String paramName;

    /**
     * Get paramType.
     *
     * @return paramType
     */
    public String getParamType() {
        return paramType;
    }

    /**
     * Set paramType.
     *
     * @param paramType paramType
     */
    public void setParamType(final String paramType) {
        this.paramType = paramType;
    }

    /**
     * Get paramName.
     *
     * @return paramName
     */
    public String getParamName() {
        return paramName;
    }

    /**
     * Set paramName.
     *
     * @param paramName paramName
     */
    public void setParamName(final String paramName) {
        this.paramName = paramName;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        StickyKeyConfig that = (StickyKeyConfig) o;
        return Objects.equals(paramType, that.paramType) && Objects.equals(paramName, that.paramName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(paramType, paramName);
    }

    @Override
    public String toString() {
        return "StickyKeyConfig{"
                + "paramType='" + paramType + '\''
                + ", paramName='" + paramName + '\''
                + '}';
    }
}
