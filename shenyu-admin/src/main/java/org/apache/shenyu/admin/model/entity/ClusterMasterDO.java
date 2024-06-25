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

package org.apache.shenyu.admin.model.entity;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Date;

/**
 * The type cluster master dto.
 */
public class ClusterMasterDO implements Serializable {

    private static final long serialVersionUID = -7115137071311176280L;

    /**
     * primary key id.
     */
    private String id;

    /**
     * the masterHost.
     */
    private String masterHost;

    /**
     * the masterPort.
     */
    private String masterPort;

    /**
     * the contextPath.
     */
    private String contextPath;

    /**
     * created time.
     */
    private Timestamp dateCreated;

    /**
     * updated time.
     */
    private Timestamp dateUpdated;

    public ClusterMasterDO() {
    }

    public ClusterMasterDO(final String id,
                           final String masterHost,
                           final String masterPort,
                           final Timestamp dateCreated,
                           final Timestamp dateUpdated) {
        this.id = id;
        this.masterHost = masterHost;
        this.masterPort = masterPort;
        this.dateCreated = dateCreated;
        this.dateUpdated = dateUpdated;
    }

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
     * Get masterHost.
     * @return masterHost
     */
    public String getMasterHost() {
        return masterHost;
    }

    /**
     * Set masterHost.
     * @param masterHost masterHost
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
     * Get the contextPath.
     * @return contextPath
     */
    public String getContextPath() {
        return contextPath;
    }
    
    /**
     * Set the contextPath.
     * @param contextPath contextPath
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
    public void setDateCreated(final Timestamp dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * getDateUpdated.
     *
     * @return dateUpdated
     */
    public Timestamp getDateUpdated() {
        return dateUpdated;
    }

    /**
     * setDateUpdated.
     *
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final Timestamp dateUpdated) {
        this.dateUpdated = dateUpdated;
    }


    /**
     * builder method.
     *
     * @return builder object.
     */
    public static ClusterMasterDOBuilder builder() {
        return new ClusterMasterDOBuilder();
    }

    public static final class ClusterMasterDOBuilder {

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
         * created time.
         */
        private Timestamp dateCreated;

        /**
         * updated time.
         */
        private Timestamp dateUpdated;

        /**
         * id.
         *
         * @param id the id.
         * @return ClusterMasterDOBuilder.
         */
        public ClusterMasterDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * master host.
         *
         * @param masterHost the master host.
         * @return ClusterMasterDOBuilder.
         */
        public ClusterMasterDOBuilder masterHost(final String masterHost) {
            this.masterHost = masterHost;
            return this;
        }

        /**
         * master port.
         *
         * @param masterPort the master port.
         * @return ClusterMasterDOBuilder.
         */
        public ClusterMasterDOBuilder masterPort(final String masterPort) {
            this.masterPort = masterPort;
            return this;
        }

        /**
         * master contextPath.
         *
         * @param contextPath the master contextPath.
         * @return ClusterMasterDOBuilder.
         */
        public ClusterMasterDOBuilder contextPath(final String contextPath) {
            this.contextPath = contextPath;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return ClusterMasterDOBuilder.
         */
        public ClusterMasterDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return ClusterMasterDOBuilder.
         */
        public ClusterMasterDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public ClusterMasterDO build() {
            ClusterMasterDO clusterMasterDO = new ClusterMasterDO();
            clusterMasterDO.setId(id);
            clusterMasterDO.setMasterHost(masterHost);
            clusterMasterDO.setMasterPort(masterPort);
            clusterMasterDO.setContextPath(contextPath);
            clusterMasterDO.setDateCreated(dateCreated);
            clusterMasterDO.setDateUpdated(dateUpdated);
            return clusterMasterDO;
        }

    }
}
