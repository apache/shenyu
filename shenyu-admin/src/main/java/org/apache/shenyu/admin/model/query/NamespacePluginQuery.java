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

import org.apache.shenyu.admin.model.page.PageParameter;

import java.io.Serializable;
import java.util.Objects;

/**
 * this is namespace plugin query.
 */
public class NamespacePluginQuery extends PluginQuery implements Serializable {

    private static final long serialVersionUID = -4156603344039606920L;

    /**
     * namespace id.
     */
    private String namespaceId;

    public NamespacePluginQuery() {
    }

    public NamespacePluginQuery(final String name, final Integer enabled, final PageParameter pageParameter, final String namespaceId) {
        super(name, enabled, pageParameter);
        this.namespaceId = namespaceId;
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
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        NamespacePluginQuery that = (NamespacePluginQuery) o;
        return Objects.equals(namespaceId, that.namespaceId);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), namespaceId);
    }
}
