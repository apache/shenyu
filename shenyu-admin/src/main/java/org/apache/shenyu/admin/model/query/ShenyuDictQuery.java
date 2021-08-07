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

/**
 * this is shenyu dict query.
 */
public class ShenyuDictQuery {
    /**
     * dict type.
     */
    private String type;

    /**
     * dict code.
     */
    private String dictCode;

    /**
     * dict name.
     */
    private String dictName;

    /**
     * page parameter.
     */
    private PageParameter pageParameter;

    public ShenyuDictQuery() {
    }

    public ShenyuDictQuery(final String type, final String dictCode, final String dictName, final PageParameter pageParameter) {
        this.type = type;
        this.dictCode = dictCode;
        this.dictName = dictName;
        this.pageParameter = pageParameter;
    }

    /**
     * Gets the value of type.
     *
     * @return the value of type
     */
    public String getType() {
        return type;
    }

    /**
     * Sets the type.
     *
     * @param type type
     */
    public void setType(final String type) {
        this.type = type;
    }

    /**
     * Gets the value of dictCode.
     *
     * @return the value of dictCode
     */
    public String getDictCode() {
        return dictCode;
    }

    /**
     * Sets the dictCode.
     *
     * @param dictCode dictCode
     */
    public void setDictCode(final String dictCode) {
        this.dictCode = dictCode;
    }

    /**
     * Gets the value of dictName.
     *
     * @return the value of dictName
     */
    public String getDictName() {
        return dictName;
    }

    /**
     * Sets the dictName.
     *
     * @param dictName dictName
     */
    public void setDictName(final String dictName) {
        this.dictName = dictName;
    }

    /**
     * Gets the value of pageParameter.
     *
     * @return the value of pageParameter
     */
    public PageParameter getPageParameter() {
        return pageParameter;
    }

    /**
     * Sets the pageParameter.
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
        if (!(o instanceof ShenyuDictQuery)) {
            return false;
        }
        ShenyuDictQuery that = (ShenyuDictQuery) o;
        return Objects.equals(type, that.type)
                && Objects.equals(dictCode, that.dictCode)
                && Objects.equals(dictName, that.dictName)
                && Objects.equals(pageParameter, that.pageParameter);
    }

    @Override
    public int hashCode() {
        return Objects.hash(type, dictCode, dictName, pageParameter);
    }
}
