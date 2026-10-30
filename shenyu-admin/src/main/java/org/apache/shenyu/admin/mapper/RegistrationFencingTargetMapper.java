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
import org.apache.shenyu.admin.model.entity.RegistrationFencingTargetDO;

import java.util.List;

/**
 * Mapper for the persisted registration fencing target state.
 */
@Mapper
public interface RegistrationFencingTargetMapper {

    /**
     * Select a target.
     *
     * @param id target id
     * @return target
     */
    RegistrationFencingTargetDO selectById(String id);

    /**
     * Select a target while holding its row lock.
     *
     * @param id target id
     * @return target
     */
    RegistrationFencingTargetDO selectByIdForUpdate(String id);

    /**
     * Select a target by its current session.
     *
     * @param domainId authority domain
     * @param sessionId session identifier
     * @return target
     */
    RegistrationFencingTargetDO selectBySessionId(@Param("domainId") String domainId,
                                                  @Param("sessionId") String sessionId);

    /**
     * Select an exact selector/address target.
     *
     * @param selectorId selector id
     * @param host host
     * @param port port
     * @return target
     */
    RegistrationFencingTargetDO selectBySelectorAddress(@Param("selectorId") String selectorId,
                                                        @Param("host") String host,
                                                        @Param("port") Integer port);

    /**
     * Select targets bound to a selector.
     *
     * @param selectorId selector id
     * @return targets
     */
    List<RegistrationFencingTargetDO> selectBySelectorId(String selectorId);

    /**
     * Select all targets matching an instance observation.
     *
     * @param namespaceId namespace id
     * @param host host
     * @param port port
     * @return matching targets
     */
    List<RegistrationFencingTargetDO> selectByNamespaceAddress(@Param("namespaceId") String namespaceId,
                                                               @Param("host") String host,
                                                               @Param("port") Integer port);

    /**
     * Select protected targets in a namespace.
     *
     * @param namespaceId namespace id
     * @return targets
     */
    List<RegistrationFencingTargetDO> selectByNamespaceId(String namespaceId);

    /**
     * Insert a target.
     *
     * @param target target
     * @return affected rows
     */
    int insert(RegistrationFencingTargetDO target);

    /**
     * Update target/session state.
     *
     * @param target target
     * @return affected rows
     */
    int update(RegistrationFencingTargetDO target);
}
