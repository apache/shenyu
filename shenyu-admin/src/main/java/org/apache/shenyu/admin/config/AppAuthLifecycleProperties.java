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

package org.apache.shenyu.admin.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

/**
 * X1 app auth properties.
 * service-users is the deployment-configured whitelist of immutable service-account userIds:
 * empty by default meaning deny all; renaming a user never changes the callerKey and the shared admin is never whitelisted implicitly.
 */
@Component
@ConfigurationProperties(prefix = "shenyu.app-auth.lifecycle")
public class AppAuthLifecycleProperties {

    /**
     * Whitelist of service-account userIds (empty by default: deny all).
     */
    private List<String> serviceUserIds = new ArrayList<>();

    /**
     * Gets the value of serviceUserIds.
     *
     * @return the value of serviceUserIds
     */
    public List<String> getServiceUserIds() {
        return serviceUserIds;
    }

    /**
     * Sets the serviceUserIds.
     *
     * @param serviceUserIds serviceUserIds
     */
    public void setServiceUserIds(final List<String> serviceUserIds) {
        this.serviceUserIds = serviceUserIds;
    }
}
