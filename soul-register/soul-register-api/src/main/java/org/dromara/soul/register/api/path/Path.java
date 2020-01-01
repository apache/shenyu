/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.register.api.path;

import org.dromara.soul.common.http.URL;
import org.dromara.soul.register.api.RegisterDirectoryListener;

/**
 * Path .
 * Determination of the unique path associated with RPC.
 *
 * @author sixh
 */
public interface Path {
    /**
     * Returns an url that the soul can read.
     *
     * @return soul path.
     */
    URL getHttpPath();

    /**
     * Gets a status message for the current service.
     * 1.If the status eq {@link RegisterDirectoryListener#REMOVE_ALL} return {@link EmptyPath}.
     * {@link RegisterDirectoryListener#ADD}
     * {@link RegisterDirectoryListener#REMOVE}
     * {@link RegisterDirectoryListener#REMOVE_ALL}
     *
     * @return status. integer
     * @see RegisterDirectoryListener
     */
    Integer status();

    /**
     * Returns an object handler that can be adapted based on the url.
     *
     * @param <T> the type parameter
     * @param url the url
     * @return the path obj.
     */
    <T extends Path> T getPathObj(URL url);

    /**
     * Returns an environment parameter.
     *
     * @return evn.
     */
    String getEvn();
}
