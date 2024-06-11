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

package org.apache.shenyu.admin.mode.cluster.service;

import org.apache.shenyu.admin.model.dto.ClusterMasterDTO;

public interface ClusterSelectMasterService {
    
    /**
     * Select the cluster master.
     * @return select result
     */
    boolean selectMaster();
    
    /**
     * Select the cluster master.
     * @param masterHost master host
     * @param masterPort master port
     * @param contextPath  context path
     * @return select result
     */
    boolean selectMaster(String masterHost, String masterPort, String contextPath);
    
    /**
     * check the cluster master status.
     * @return check result
     * @throws IllegalStateException Illegal State Exception
     */
    boolean checkMasterStatus() throws IllegalStateException;
    
    /**
     * Release the cluster master.
     * @return release result
     */
    boolean releaseMaster();
    
    /**
     * Whether this node is the cluster master.
     * @return is master
     */
    boolean isMaster();
    
    /**
     * Get master.
     * @return ClusterMasterDTO
     */
    ClusterMasterDTO getMaster();
    
    /**
     * Get master url.
     * @return master url
     */
    String getMasterUrl();

}
