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

package org.apache.shenyu.plugin.grpc.resolver;

import org.springframework.beans.BeanUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * Shenyu service instance list.
 */
public class ShenyuServiceInstanceLists {

    private CopyOnWriteArrayList<ShenyuServiceInstance> shenyuServiceInstances;

    private String appName;

    /**
     * Instantiates a new Shenyu service instance lists.
     */
    public ShenyuServiceInstanceLists() {
    }

    /**
     * Instantiates a new Shenyu service instance lists.
     *
     * @param shenyuServiceInstances the shenyu service instances
     * @param appName                the app name
     */
    public ShenyuServiceInstanceLists(final CopyOnWriteArrayList<ShenyuServiceInstance> shenyuServiceInstances, final String appName) {
        this.shenyuServiceInstances = shenyuServiceInstances;
        this.appName = appName;
    }

    /**
     * Gets shenyu service instances.
     *
     * @return the shenyu service instances
     */
    public CopyOnWriteArrayList<ShenyuServiceInstance> getShenyuServiceInstances() {
        return shenyuServiceInstances;
    }

    /**
     * Sets shenyu service instances.
     *
     * @param shenyuServiceInstances the shenyu service instances
     */
    public void setShenyuServiceInstances(final CopyOnWriteArrayList<ShenyuServiceInstance> shenyuServiceInstances) {
        this.shenyuServiceInstances = shenyuServiceInstances;
    }

    /**
     * Gets app name.
     *
     * @return the app name
     */
    public String getAppName() {
        return appName;
    }

    /**
     * Sets app name.
     *
     * @param appName the app name
     */
    public void setAppName(final String appName) {
        this.appName = appName;
    }

    /**
     * Get instance copy.
     *
     * @return list list
     */
    public List<ShenyuServiceInstance> getCopyInstances() {
        List<ShenyuServiceInstance> copy = new ArrayList<>(shenyuServiceInstances.size());
        shenyuServiceInstances.forEach(instance -> {
            ShenyuServiceInstance cp = new ShenyuServiceInstance();
            BeanUtils.copyProperties(instance, cp);
            copy.add(cp);
        });
        return copy;
    }
}
