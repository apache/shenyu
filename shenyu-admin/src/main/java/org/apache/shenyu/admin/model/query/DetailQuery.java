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

public class DetailQuery {


    /**
     * field value.
     */
    private String fieldValue;

    /**
     * value desc.
     */
    private String valueDesc;

    /**
     * pageParameter.
     */
    private PageParameter pageParameter;

    public DetailQuery(final String fieldValue, final String valueDesc, final PageParameter pageParameter) {
        this.fieldValue = fieldValue;
        this.valueDesc = valueDesc;
        this.pageParameter = pageParameter;
    }

    /**
     * get fieldValue.
     *
     * @return fieldValue
     */
    public String getFieldValue() {
        return fieldValue;
    }

    /**
     * set fieldValue.
     *
     * @param fieldValue fieldValue
     */
    public void setFieldValue(final String fieldValue) {
        this.fieldValue = fieldValue;
    }

    /**
     * get valueDesc.
     *
     * @return valueDesc
     */
    public String getValueDesc() {
        return valueDesc;
    }

    /**
     * set valueDesc.
     *
     * @param valueDesc valueDesc
     */
    public void setValueDesc(final String valueDesc) {
        this.valueDesc = valueDesc;
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
        DetailQuery that = (DetailQuery) o;
        return Objects.equals(fieldValue, that.fieldValue)
                && Objects.equals(valueDesc, that.valueDesc)
                && Objects.equals(pageParameter, that.pageParameter);
    }

    @Override
    public int hashCode() {
        return Objects.hash(fieldValue, valueDesc, pageParameter);
    }
}
