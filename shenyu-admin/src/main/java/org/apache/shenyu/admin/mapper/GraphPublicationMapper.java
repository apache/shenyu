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

package org.apache.shenyu.admin.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.shenyu.admin.model.entity.GraphDraftDO;
import org.apache.shenyu.admin.model.entity.GraphRevisionDO;

import java.util.List;

/**
 * Namespace-scoped authoring state and immutable graph publications.
 */
@Mapper
public interface GraphPublicationMapper {

    /**
     * Load the mutable head of a graph.
     *
     * @param namespaceId namespace identifier
     * @param graphId graph identifier
     * @return head, or null for an unknown graph
     */
    GraphDraftDO findDraft(@Param("namespaceId") String namespaceId, @Param("graphId") String graphId);

    /**
     * Create a head owned by an existing namespace.
     *
     * @param draft initial draft
     * @return number of inserted heads, zero if the namespace is missing
     */
    int insertDraft(GraphDraftDO draft);

    /**
     * Replace a head if it still identifies the versions read by the caller.
     *
     * @param draft new authoring state and committed revision pointer
     * @param expectedDraftVersion last observed draft version
     * @param expectedPublishedRevision last observed publication
     * @return number of updated rows
     */
    int updateDraft(@Param("draft") GraphDraftDO draft, @Param("expectedDraftVersion") long expectedDraftVersion,
                    @Param("expectedPublishedRevision") long expectedPublishedRevision);

    /**
     * Append a publication to the history of this graph.
     *
     * @param revision immutable publication
     * @return number of inserted rows
     */
    int insertRevision(GraphRevisionDO revision);

    /**
     * Read one historical publication.
     *
     * @param namespaceId namespace identifier
     * @param graphId graph identifier
     * @param revision publication sequence number
     * @return publication, or null if absent
     */
    GraphRevisionDO findRevision(@Param("namespaceId") String namespaceId, @Param("graphId") String graphId, @Param("revision") long revision);

    /**
     * Read immutable publications in increasing revision order.
     *
     * @param namespaceId namespace identifier
     * @param graphId graph identifier
     * @return publication history
     */
    List<GraphRevisionDO> listRevisions(@Param("namespaceId") String namespaceId, @Param("graphId") String graphId);

    /**
     * Reconstruct the latest committed publications, including deletions.
     *
     * @param namespaceId namespace identifier
     * @return latest publication per graph, ordered by graph identifier
     */
    List<GraphRevisionDO> listPublished(@Param("namespaceId") String namespaceId);

    /**
     * Find plugin names available to authoring validation in the namespace.
     *
     * @param namespaceId namespace identifier
     * @return enabled plugin names
     */
    List<String> listEnabledPluginNames(@Param("namespaceId") String namespaceId);
}
