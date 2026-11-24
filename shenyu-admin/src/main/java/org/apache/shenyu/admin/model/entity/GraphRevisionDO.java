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

package org.apache.shenyu.admin.model.entity;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonRawValue;

/**
 * One entry in the append-only publication history of a namespace graph.
 */
public final class GraphRevisionDO {

    private String namespaceId;

    private String graphId;

    private long revision;

    private String graphJson;

    /**
     * Get the namespace identifier.
     *
     * @return namespace identifier
     */
    public String getNamespaceId() {
        return namespaceId;
    }

    /**
     * Set the namespace identifier.
     *
     * @param namespaceId namespace identifier
     */
    public void setNamespaceId(final String namespaceId) {
        this.namespaceId = namespaceId;
    }

    /**
     * Get the graph identifier.
     *
     * @return graph identifier
     */
    public String getGraphId() {
        return graphId;
    }

    /**
     * Set the graph identifier.
     *
     * @param graphId graph identifier
     */
    public void setGraphId(final String graphId) {
        this.graphId = graphId;
    }

    /**
     * Get the publication sequence number.
     *
     * @return publication sequence number
     */
    public long getRevision() {
        return revision;
    }

    /**
     * Set the publication sequence number.
     *
     * @param revision publication sequence number
     */
    public void setRevision(final long revision) {
        this.revision = revision;
    }

    /**
     * Get the published definition, or null for a deletion.
     *
     * @return published definition, or null for a deletion
     */
    @JsonProperty("graph")
    @JsonRawValue
    public String getGraphJson() {
        return graphJson;
    }

    /**
     * Set the published definition, or null for a deletion.
     *
     * @param graphJson published definition, or null for a deletion
     */
    public void setGraphJson(final String graphJson) {
        this.graphJson = graphJson;
    }
}
