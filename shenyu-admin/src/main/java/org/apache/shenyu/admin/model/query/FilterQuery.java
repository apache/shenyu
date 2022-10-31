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

package org.apache.shenyu.admin.model.query;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;

/**
 * data permission filter query.
 */
public class FilterQuery implements Serializable {

    private static final long serialVersionUID = 9107238465094879060L;

    /**
     * filter ids.
     */
    private List<String> filterIds;

    /**
     * Gets the value of filterIds.
     *
     * @return the value of filterIds
     */
    public List<String> getFilterIds() {
        return filterIds;
    }

    /**
     * Sets the filterIds.
     *
     * @param filterIds filterIds
     */
    public void setFilterIds(final List<String> filterIds) {
        this.filterIds = filterIds;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof FilterQuery)) {
            return false;
        }
        FilterQuery that = (FilterQuery) o;
        return Objects.equals(filterIds, that.filterIds);
    }

    @Override
    public int hashCode() {
        return Objects.hash(filterIds);
    }
}
