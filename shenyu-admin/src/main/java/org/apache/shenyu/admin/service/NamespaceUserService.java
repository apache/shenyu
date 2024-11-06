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

import org.apache.shenyu.admin.model.query.NamespaceUserQuery;
import org.apache.shenyu.admin.model.vo.NamespaceUserRelVO;

import java.util.List;

/**
 * this is namespace user service.
 */
public interface NamespaceUserService extends PageService<NamespaceUserQuery, NamespaceUserRelVO> {

    /**
     * create namespace user rel.
     *
     * @param namespaceId        namespaceId.
     * @param userId           userId.
     * @return namespaceUserVO
     */
    NamespaceUserRelVO create(String namespaceId, String userId);
    
    /**
     * list namespace id by user id.
     *
     * @param userId userId.
     * @return namespace id list.
     */
    List<String> listNamespaceIdByUserId(String userId);

}
