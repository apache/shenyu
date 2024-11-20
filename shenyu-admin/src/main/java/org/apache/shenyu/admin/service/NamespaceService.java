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

import org.apache.shenyu.admin.model.dto.NamespaceDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.NamespaceQuery;
import org.apache.shenyu.admin.model.vo.NamespaceVO;

import java.util.List;

public interface NamespaceService {

    /**
     * Create or update namespace.
     *
     * @param namespaceDTO the namespace dto
     * @return the string
     */
    NamespaceVO createOrUpdate(NamespaceDTO namespaceDTO);

    /**
     * find page of namespace by query.
     *
     * @param namespaceQuery {@linkplain NamespaceQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<NamespaceVO> listByPage(NamespaceQuery namespaceQuery);

    /**
     * delete by id.
     *
     * @param ids ids
     * @return msg
     */
    String delete(List<String> ids);

    /**
     * find namespace by id.
     *
     * @param namespaceId pk.
     * @return {@linkplain NamespaceVO}
     */
    NamespaceVO findById(String namespaceId);

    /**
     * find namespace by namespace id.
     *
     * @param namespaceId pk.
     * @return {@linkplain NamespaceVO}
     */
    NamespaceVO findByNamespaceId(String namespaceId);

    /**
     * find list of namespace.
     *
     * @param name name
     * @return {@linkplain List}
     */
    List<NamespaceVO> list(String name);

    /**
     * find list of namespace.
     *
     * @return {@linkplain List}
     */
    List<NamespaceVO> listAll();
}
