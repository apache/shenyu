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

package org.apache.shenyu.common.dto.convert.rule;

import java.util.Objects;

/**
 * this is RequestHandle plugin handle.
 */
public class GeneralContextHandle {
    /**
     * general context type.
     */
    private String generalContextType;

    /**
     * generalContextKey.
     * <p>
     * need to be added new context value.
     * key: new general context key, value: general context value.
     * </p>
     */
    private String generalContextKey;

    /**
     * generalContextValue.
     * when generalContextType is addGeneralContext, the generalContextValue is the value of generalContextKey.
     * when generalContextType is transmitHeaderToGeneralContext, the generalContextValue is the new key of generalContext.
     * In this case, if generalContextValue is blank, default value is same as generalContextKey.
     */
    private String generalContextValue;

    /**
     * no args constructor.
     */
    public GeneralContextHandle() {
    }

    /**
     * all args constructor.
     *
     * @param generalContextType  generalContextType
     * @param generalContextKey   generalContextKey
     * @param generalContextValue generalContextValue
     */
    public GeneralContextHandle(final String generalContextType, final String generalContextKey, final String generalContextValue) {
        this.generalContextType = generalContextType;
        this.generalContextKey = generalContextKey;
        this.generalContextValue = generalContextValue;
    }

    /**
     * get generalContextType.
     *
     * @return generalContextType
     */
    public String getGeneralContextType() {
        return generalContextType;
    }

    /**
     * set generalContextType.
     *
     * @param generalContextType generalContextType
     */
    public void setGeneralContextType(final String generalContextType) {
        this.generalContextType = generalContextType;
    }

    /**
     * get generalContextKey.
     *
     * @return generalContextKey
     */
    public String getGeneralContextKey() {
        return generalContextKey;
    }

    /**
     * set generalContextKey.
     *
     * @param generalContextKey generalContextKey
     */
    public void setGeneralContextKey(final String generalContextKey) {
        this.generalContextKey = generalContextKey;
    }

    /**
     * get generalContextValue.
     *
     * @return generalContextValue
     */
    public String getGeneralContextValue() {
        return generalContextValue;
    }

    /**
     * set generalContextValue.
     *
     * @param generalContextValue generalContextValue
     */
    public void setGeneralContextValue(final String generalContextValue) {
        this.generalContextValue = generalContextValue;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        GeneralContextHandle that = (GeneralContextHandle) o;
        return Objects.equals(generalContextType, that.generalContextType)
                && Objects.equals(generalContextKey, that.generalContextKey)
                && Objects.equals(generalContextValue, that.generalContextValue);
    }

    @Override
    public int hashCode() {
        return Objects.hash(generalContextType, generalContextKey, generalContextValue);
    }

    @Override
    public String toString() {
        return "GeneralContextHandleContent{"
                + "generalContextType='" + generalContextType + '\''
                + ", generalContextKey='" + generalContextKey + '\''
                + ", generalContextValue='" + generalContextValue + '\''
                + '}';
    }
}
