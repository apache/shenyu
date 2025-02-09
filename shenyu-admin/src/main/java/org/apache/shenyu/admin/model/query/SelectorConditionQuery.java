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

package org.apache.shenyu.admin.model.query;

import java.io.Serializable;
import java.util.Objects;

/**
 * this is selector condition query.
 */
public class SelectorConditionQuery implements Serializable {

    private static final long serialVersionUID = 9107238465094879060L;

    /**
     * selector id.
     */
    private String selectorId;

    /**
     * namespace id.
     */
    private String namespaceId;

    public SelectorConditionQuery() {
    }

    public SelectorConditionQuery(final String selectorId) {
        this.selectorId = selectorId;
    }

    /**
     * Gets the value of selectorId.
     *
     * @return the value of selectorId
     */
    public String getSelectorId() {
        return selectorId;
    }

    /**
     * Sets the selectorId.
     *
     * @param selectorId selectorId
     */
    public void setSelectorId(final String selectorId) {
        this.selectorId = selectorId;
    }

    /**
     * Gets the value of name.
     *
     * @return the value of name
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * Sets the namespaceId.
     *
     * @param namespaceId namespaceId
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        SelectorConditionQuery that = (SelectorConditionQuery) o;
        return Objects.equals(selectorId, that.selectorId) && Objects.equals(namespaceId, that.namespaceId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(selectorId, namespaceId);
    }
}
