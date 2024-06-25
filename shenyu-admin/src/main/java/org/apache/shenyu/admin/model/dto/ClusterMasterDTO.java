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

package org.apache.shenyu.admin.model.dto;

import java.io.Serializable;
import java.util.Date;
import java.util.Objects;

/**
 * The type cluster master dto.
 */
public class ClusterMasterDTO implements Serializable {

    private static final long serialVersionUID = 803678746937608497L;

    /**
     * primary key id.
     */
    private String id;

    /**
     * the master host.
     */
    private String masterHost;

    /**
     * the master port.
     */
    private String masterPort;

    /**
     * the master contextPath.
     */
    private String contextPath;

    /**
     * create time.
     */
    private Date dateCreated;

    /**
     * update time.
     */
    private Date dateUpdated;

    /**
     * getId.
     *
     * @return id
     */
    public String getId() {
        return id;
    }

    /**
     * set id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * Get master host.
     * @return master host
     */
    public String getMasterHost() {
        return masterHost;
    }

    /**
     * Set master host.
     * @param masterHost master host
     */
    public void setMasterHost(final String masterHost) {
        this.masterHost = masterHost;
    }

    /**
     * Get master port.
     * @return master port
     */
    public String getMasterPort() {
        return masterPort;
    }

    /**
     * Set master port.
     * @param masterPort master port
     */
    public void setMasterPort(final String masterPort) {
        this.masterPort = masterPort;
    }
    
    /**
     * Get context path.
     * @return context path
     */
    public String getContextPath() {
        return contextPath;
    }
    
    /**
     * Set context path.
     * @param contextPath context path
     */
    public void setContextPath(final String contextPath) {
        this.contextPath = contextPath;
    }
    
    /**
     * getDateCreated.
     *
     * @return dateCreated
     */
    public Date getDateCreated() {
        return dateCreated;
    }

    /**
     * setDateCreated.
     *
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final Date dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * getDateUpdated.
     *
     * @return dateUpdated
     */
    public Date getDateUpdated() {
        return dateUpdated;
    }

    /**
     * setDateUpdated.
     *
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final Date dateUpdated) {
        this.dateUpdated = dateUpdated;
    }
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof ClusterMasterDTO)) {
            return false;
        }
        ClusterMasterDTO apiDTO = (ClusterMasterDTO) o;
        return Objects.equals(id, apiDTO.id)
                && Objects.equals(masterHost, apiDTO.masterHost)
                && Objects.equals(masterPort, apiDTO.masterPort)
                && Objects.equals(dateCreated, apiDTO.dateCreated)
                && Objects.equals(dateUpdated, apiDTO.dateUpdated);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, masterHost, masterPort, dateCreated, dateUpdated);
    }

}
