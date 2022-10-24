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
import org.apache.shenyu.admin.model.dto.TagDTO;
import org.apache.shenyu.admin.model.vo.RoleVO;
import org.apache.shenyu.admin.model.vo.TagVO;

/**
 * The interface tag service.
 */
public interface TagService {

    /**
     * create or update tag.
     *
     * @param tagDTO {@linkplain TagDTO}
     * @return rows int
     */
    int create(TagDTO tagDTO);

    /**
     * create or update tag.
     *
     * @param tagDTO {@linkplain TagDTO}
     * @return rows int
     */
    int update(TagDTO tagDTO);

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
     * @return {@linkplain TagVO}
     */
    TagVO findById(String id);

    /**
     * find tag by tagName.
     *
     * @param tagName tag name
     * @return {@linkplain RoleVO}
     */
    List<TagVO> findByQuery(String tagName);

    /**
     * find tag by parentTagId.
     * @param parentTagId ta
     * @return {@linkplain TagVO}
     */
    List<TagVO> findByParentTagId(String parentTagId);

}
