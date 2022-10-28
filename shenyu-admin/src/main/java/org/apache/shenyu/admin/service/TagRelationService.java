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

package org.apache.shenyu.admin.service;

import java.util.List;
import org.apache.shenyu.admin.model.dto.TagRelationDTO;
import org.apache.shenyu.admin.model.entity.TagRelationDO;

/**
 * The interface tag relation service.
 */
public interface TagRelationService {

    /**
     * create or update tag.
     *
     * @param tagRelationDTO {@linkplain TagRelationDTO}
     * @return rows int
     */
    int create(TagRelationDTO tagRelationDTO);

    /**
     * create or update tag.
     *
     * @param tagRelationDTO {@linkplain TagRelationDTO}
     * @return rows int
     */
    int update(TagRelationDTO tagRelationDTO);

    /**
     * delete roles.
     *
     * @param ids primary key
     * @return rows int
     */
    int delete(List<String> ids);

    /**
     * find tag by id.
     *
     * @param id primary key
     * @return {@linkplain TagRelationDO}
     */
    TagRelationDO findById(String id);

    /**
     * find tag by tagId.
     *
     * @param tagId tag Id
     * @return {@linkplain TagRelationDO}
     */
    List<TagRelationDO> findByTagId(String tagId);

    /**
     * find tag relation by apiId.
     * @param apiId apiId
     * @return {@linkplain TagRelationDO}
     */
    List<TagRelationDO> findApiId(String apiId);
}
