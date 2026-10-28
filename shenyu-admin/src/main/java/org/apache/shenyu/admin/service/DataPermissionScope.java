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

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Effective data permission scope of a dashboard caller.
 * <p>The scope is one of:</p>
 * <ul>
 *     <li>unrestricted: a global administrator may read every resource;</li>
 *     <li>selected: only the granted resource ids are readable;</li>
 *     <li>none: the caller has no grants, so no resource is readable.</li>
 * </ul>
 * <p>An empty grant set must never degrade to an unrestricted scope.</p>
 */
public final class DataPermissionScope {

    private static final DataPermissionScope UNRESTRICTED = new DataPermissionScope(true, Collections.emptyList());

    private final boolean unrestricted;

    private final List<String> dataIds;

    private DataPermissionScope(final boolean unrestricted, final List<String> dataIds) {
        this.unrestricted = unrestricted;
        this.dataIds = dataIds;
    }

    /**
     * Create the unrestricted scope used by a global administrator.
     *
     * @return unrestricted scope
     */
    public static DataPermissionScope unrestricted() {
        return UNRESTRICTED;
    }

    /**
     * Create a scope restricted to the given granted resource ids.
     *
     * @param dataIds granted resource ids; an empty or null list means no resource is readable
     * @return restricted scope
     */
    public static DataPermissionScope of(final List<String> dataIds) {
        if (Objects.isNull(dataIds)) {
            return new DataPermissionScope(false, Collections.emptyList());
        }
        return new DataPermissionScope(false, dataIds.stream().filter(Objects::nonNull).collect(Collectors.toUnmodifiableList()));
    }

    /**
     * Whether the scope is unrestricted.
     *
     * @return true for a global administrator
     */
    public boolean isUnrestricted() {
        return unrestricted;
    }

    /**
     * Whether the restricted scope exposes no resource at all.
     *
     * @return true when the caller has no grants
     */
    public boolean isNone() {
        return !unrestricted && dataIds.isEmpty();
    }

    /**
     * Get the granted resource ids.
     *
     * @return granted resource ids, empty when {@link #isNone()} is true
     */
    public List<String> getDataIds() {
        return dataIds;
    }
}
