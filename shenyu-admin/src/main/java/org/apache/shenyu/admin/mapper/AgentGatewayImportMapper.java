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
import org.apache.shenyu.admin.model.entity.AgentGatewayImportDO;

import java.sql.Timestamp;

/** Admin-only provenance; never synchronized into runtime tool plans. Writers lock the parent rule. */
@Mapper
public interface AgentGatewayImportMapper {

    /**
     * Read the durable manifest of one target rule.
     * @param ruleId target rule
     * @param namespaceId exact namespace
     * @return the ownership record or null
     */
    AgentGatewayImportDO selectByRuleId(@Param("ruleId") String ruleId, @Param("namespaceId") String namespaceId);

    /**
     * Insert the record in the same transaction as RuleService publication.
     * @param record bounded ownership state
     * @return affected rows
     */
    int insertSelective(AgentGatewayImportDO record);

    /**
     * Replace the manifest while holding the target rule lock, preserving id and creation time.
     * @param id primary key
     * @param manifest bounded ownership state
     * @param dateUpdated update time
     * @return affected rows
     */
    int updateManifestById(@Param("id") String id, @Param("manifest") String manifest, @Param("dateUpdated") Timestamp dateUpdated);

    /**
     * Remove the record of one target rule.
     * @param ruleId target rule
     * @param namespaceId exact namespace
     * @return affected rows
     */
    int deleteByRuleId(@Param("ruleId") String ruleId, @Param("namespaceId") String namespaceId);
}
