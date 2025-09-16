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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.common.enums.DataEventTypeEnum;

/**
 * The interface Sync data service.
 */
public interface SyncDataService {

    /**
     * Sync all boolean.
     *
     * @param type the type
     * @return the boolean
     */
    boolean syncAll(DataEventTypeEnum type);

    /**
     * Sync all by namespaceId boolean.
     *
     * @param type the type
     * @param namespaceId the namespaceId
     * @return the boolean
     */
    boolean syncAllByNamespaceId(DataEventTypeEnum type, String namespaceId);

    /**
     * Sync plugin data boolean.
     *
     * @param id    the namespace plugin id
     * @return the boolean
     */
    boolean syncPluginData(String id);

    /**
     * Sync plugin data boolean.
     *
     * @param namespaceId the namespace id
     * @param pluginId the plugin id
     * @return the boolean
     */
    boolean syncPluginData(String namespaceId, String pluginId);
}
