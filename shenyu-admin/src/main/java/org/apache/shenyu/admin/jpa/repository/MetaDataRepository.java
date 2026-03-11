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

package org.apache.shenyu.admin.jpa.repository;

import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.validation.ExistProvider;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.io.Serializable;
import java.util.List;
import java.util.Optional;

/**
 * The interface Meta data repository.
 */
@Repository
public interface MetaDataRepository extends JpaRepository<MetaDataDO, String>, ExistProvider {

    @Override
    default Boolean existed(Serializable key) {
        return existsById((String) key);
    }

    /**
     * Find all by namespace id.
     *
     * @param namespaceId the namespace id
     * @return the list
     */
    List<MetaDataDO> findByNamespaceId(String namespaceId);

    /**
     * Find by path and namespace id.
     *
     * @param path        the path
     * @param namespaceId the namespace id
     * @return the optional
     */
    Optional<MetaDataDO> findByPathAndNamespaceId(String path, String namespaceId);

    /**
     * Find by service name and method name and namespace id.
     *
     * @param serviceName the service name
     * @param methodName  the method name
     * @param namespaceId the namespace id
     * @return the list
     */
    List<MetaDataDO> findByServiceNameAndMethodNameAndNamespaceId(String serviceName, String methodName, String namespaceId);
}
