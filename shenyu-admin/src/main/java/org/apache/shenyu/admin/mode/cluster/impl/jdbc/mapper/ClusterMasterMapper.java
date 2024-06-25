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

package org.apache.shenyu.admin.mode.cluster.impl.jdbc.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.shenyu.admin.model.entity.ClusterMasterDO;

/**
 * The interface Cluster Master mapper.
 */
@Mapper
public interface ClusterMasterMapper {

    /**
     * insert cluster master.
     *
     * @param clusterMasterDO {@linkplain ClusterMasterDO}
     * @return rows int
     */
    int insert(ClusterMasterDO clusterMasterDO);

    /**
     * update cluster master.
     *
     * @param clusterMasterDO {@linkplain ClusterMasterDO}
     * @return rows int
     */
    int updateSelective(ClusterMasterDO clusterMasterDO);

    /**
     * Count with condition.
     *
     * @param clusterMasterDO condition
     * @return The value of count result
     */
    long count(ClusterMasterDO clusterMasterDO);

    /**
     * select by id.
     *
     * @param id primary key.
     * @return ClusterMasterDO
     */
    ClusterMasterDO selectById(String id);

}
