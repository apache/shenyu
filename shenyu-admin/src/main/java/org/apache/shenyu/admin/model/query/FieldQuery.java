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

import org.apache.shenyu.admin.model.page.PageParameter;

import java.util.Objects;

public class FieldQuery {

    /**
     * the fieldName.
     */
    private String name;

    /**
     * the field desc.
     */
    private String fieldDesc;

    /**
     * page parameter.
     */
    private PageParameter pageParameter;

    public FieldQuery() {
    }

    public FieldQuery(final String name, final String fieldDesc, final PageParameter pageParameter) {
        this.name = name;
        this.fieldDesc = fieldDesc;
        this.pageParameter = pageParameter;
    }

    /**
     * getName.
     *
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * set name.
     *
     * @param name name.
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * getFieldDesc.
     *
     * @return fieldDesc.
     */
    public String getFieldDesc() {
        return fieldDesc;
    }

    /**
     * set field desc.
     *
     * @param fieldDesc fieldDesc
     */
    public void setFieldDesc(final String fieldDesc) {
        this.fieldDesc = fieldDesc;
    }


    /**
     * getPageParameter.
     *
     * @return PageParameter
     */
    public PageParameter getPageParameter() {
        return pageParameter;
    }

    /**
     * setPageParameter.
     *
     * @param pageParameter pageParameter
     */
    public void setPageParameter(final PageParameter pageParameter) {
        this.pageParameter = pageParameter;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        FieldQuery fieldDO = (FieldQuery) o;
        return Objects.equals(name, fieldDO.name)
                && Objects.equals(fieldDesc, fieldDO.fieldDesc);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), name, fieldDesc);
    }

}
