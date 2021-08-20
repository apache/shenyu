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

import java.io.Serializable;
import java.util.Objects;

/**
 * this is application authority query.
 */
public class AppAuthQuery implements Serializable {

    private static final long serialVersionUID = -5002345943248288249L;

    /**
     * application key.
     */
    private String appKey;

    /**
     * phone.
     */
    private String phone;

    /**
     * page parameter.
     */
    private PageParameter pageParameter;

    public AppAuthQuery() {
    }

    public AppAuthQuery(final String appKey, final String phone, final PageParameter pageParameter) {
        this.appKey = appKey;
        this.phone = phone;
        this.pageParameter = pageParameter;
    }

    /**
     * Gets the value of appKey.
     *
     * @return the value of appKey
     */
    public String getAppKey() {
        return appKey;
    }

    /**
     * Sets the appKey.
     *
     * @param appKey appKey
     */
    public void setAppKey(final String appKey) {
        this.appKey = appKey;
    }

    /**
     * Gets the value of phone.
     *
     * @return the value of phone
     */
    public String getPhone() {
        return phone;
    }

    /**
     * Sets the phone.
     *
     * @param phone phone
     */
    public void setPhone(final String phone) {
        this.phone = phone;
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
        if (!(o instanceof AppAuthQuery)) {
            return false;
        }
        AppAuthQuery that = (AppAuthQuery) o;
        return Objects.equals(appKey, that.appKey) && Objects.equals(phone, that.phone) && Objects.equals(pageParameter, that.pageParameter);
    }

    @Override
    public int hashCode() {
        return Objects.hash(appKey, phone, pageParameter);
    }
}
