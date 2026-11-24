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
 * Mutable authoring state and committed revision pointer for one namespace graph.
 */
public final class GraphDraftDO {

    private String namespaceId;

    private String graphId;

    private long draftVersion;

    private String draftJson;

    private long publishedRevision;

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
     * Get the optimistic edit version.
     *
     * @return optimistic edit version
     */
    public long getDraftVersion() {
        return draftVersion;
    }

    /**
     * Set the optimistic edit version.
     *
     * @param draftVersion optimistic edit version
     */
    public void setDraftVersion(final long draftVersion) {
        this.draftVersion = draftVersion;
    }

    /**
     * Get the editable graph definition.
     *
     * @return editable graph definition
     */
    @JsonProperty("graph")
    @JsonRawValue
    public String getDraftJson() {
        return draftJson;
    }

    /**
     * Set the editable graph definition.
     *
     * @param draftJson editable graph definition
     */
    public void setDraftJson(final String draftJson) {
        this.draftJson = draftJson;
    }

    /**
     * Get the latest committed revision, or zero before publication.
     *
     * @return latest committed revision, or zero before publication
     */
    public long getPublishedRevision() {
        return publishedRevision;
    }

    /**
     * Set the latest committed revision, or zero before publication.
     *
     * @param publishedRevision latest committed revision, or zero before publication
     */
    public void setPublishedRevision(final long publishedRevision) {
        this.publishedRevision = publishedRevision;
    }
}
