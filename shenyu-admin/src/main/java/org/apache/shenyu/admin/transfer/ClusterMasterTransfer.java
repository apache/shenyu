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

package org.apache.shenyu.admin.transfer;

import org.apache.shenyu.admin.model.dto.ClusterMasterDTO;
import org.apache.shenyu.admin.model.entity.ClusterMasterDO;

/**
 * The interface Cluster Master transfer.
 */
public enum ClusterMasterTransfer {

    /**
     * The constant INSTANCE.
     */
    INSTANCE;

    /**
     * Map to entity meta data do.
     *
     * @param clusterMasterDO the meta data dto
     * @return the meta data do
     */
    public ClusterMasterDTO mapToDTO(final ClusterMasterDO clusterMasterDO) {
        ClusterMasterDTO clusterMasterDTO = new ClusterMasterDTO();
        clusterMasterDTO.setId(clusterMasterDO.getId());
        clusterMasterDTO.setMasterHost(clusterMasterDO.getMasterHost());
        clusterMasterDTO.setMasterPort(clusterMasterDO.getMasterPort());
        clusterMasterDTO.setContextPath(clusterMasterDO.getContextPath());
        clusterMasterDTO.setDateCreated(clusterMasterDO.getDateCreated());
        clusterMasterDTO.setDateUpdated(clusterMasterDO.getDateUpdated());
        return clusterMasterDTO;
    }

}
