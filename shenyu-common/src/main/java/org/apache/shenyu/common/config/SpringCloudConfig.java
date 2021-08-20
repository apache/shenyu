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

package org.apache.shenyu.common.config;

import java.io.Serializable;
import java.util.Objects;

/**
 * The springCloud plugin configuration for eureka.
 */
public class SpringCloudConfig implements Serializable {

    private static final long serialVersionUID = -5750232000945431295L;

    private String serviceUrl;

    private Integer leaseRenewalIntervalInSeconds;

    private Integer leaseExpirationDurationInSeconds;

    /**
     * get serviceUrl.
     *
     * @return serviceUrl
     */
    public String getServiceUrl() {
        return serviceUrl;
    }

    /**
     * set serviceUrl.
     *
     * @param serviceUrl serviceUrl
     */
    public void setServiceUrl(final String serviceUrl) {
        this.serviceUrl = serviceUrl;
    }

    /**
     * get leaseRenewalIntervalInSeconds.
     *
     * @return leaseRenewalIntervalInSeconds
     */
    public Integer getLeaseRenewalIntervalInSeconds() {
        return leaseRenewalIntervalInSeconds;
    }

    /**
     * set leaseRenewalIntervalInSeconds.
     *
     * @param leaseRenewalIntervalInSeconds leaseRenewalIntervalInSeconds
     */
    public void setLeaseRenewalIntervalInSeconds(final Integer leaseRenewalIntervalInSeconds) {
        this.leaseRenewalIntervalInSeconds = leaseRenewalIntervalInSeconds;
    }

    /**
     * get leaseExpirationDurationInSeconds.
     *
     * @return leaseExpirationDurationInSeconds
     */
    public Integer getLeaseExpirationDurationInSeconds() {
        return leaseExpirationDurationInSeconds;
    }

    /**
     * set leaseExpirationDurationInSeconds.
     *
     * @param leaseExpirationDurationInSeconds leaseExpirationDurationInSeconds
     */
    public void setLeaseExpirationDurationInSeconds(final Integer leaseExpirationDurationInSeconds) {
        this.leaseExpirationDurationInSeconds = leaseExpirationDurationInSeconds;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        SpringCloudConfig that = (SpringCloudConfig) o;
        return Objects.equals(serviceUrl, that.serviceUrl) && Objects.equals(leaseRenewalIntervalInSeconds, that.leaseRenewalIntervalInSeconds)
                && Objects.equals(leaseExpirationDurationInSeconds, that.leaseExpirationDurationInSeconds);
    }

    @Override
    public int hashCode() {
        return Objects.hash(serviceUrl, leaseRenewalIntervalInSeconds, leaseExpirationDurationInSeconds);
    }

    @Override
    public String toString() {
        return "SpringCloudConfig{"
                + "serviceUrl='"
                + serviceUrl
                + '\''
                + ", leaseRenewalIntervalInSeconds="
                + leaseRenewalIntervalInSeconds
                + ", leaseExpirationDurationInSeconds="
                + leaseExpirationDurationInSeconds
                + '}';
    }
}
