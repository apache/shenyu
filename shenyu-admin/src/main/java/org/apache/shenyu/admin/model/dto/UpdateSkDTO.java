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

package org.apache.shenyu.admin.model.dto;

import org.apache.shenyu.admin.service.provider.AppKeyProvider;
import org.apache.shenyu.admin.validation.annotation.Existed;

import jakarta.validation.constraints.NotBlank;
import java.io.Serializable;
import java.util.Objects;

/**
 * this is update app secret dto.
 */
public class UpdateSkDTO implements Serializable {

    private static final long serialVersionUID = -1L;

    /**
     * application key.
     */
    @NotBlank(message = "app key not null")
    @Existed(message = "app key not existed", provider = AppKeyProvider.class)
    private String appKey;

    /**
     * encryption secret.
     */
    @NotBlank(message = "app secret not null")
    private String appSecret;

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
     * Gets the value of appSecret.
     *
     * @return the value of appSecret
     */
    public String getAppSecret() {
        return appSecret;
    }

    /**
     * Sets the appSecret.
     *
     * @param appSecret appSecret
     */
    public void setAppSecret(final String appSecret) {
        this.appSecret = appSecret;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof UpdateSkDTO)) {
            return false;
        }
        UpdateSkDTO that = (UpdateSkDTO) o;
        return Objects.equals(appKey, that.appKey)
                && Objects.equals(appSecret, that.appSecret);
    }

    @Override
    public int hashCode() {
        return Objects.hash(appKey, appSecret);
    }
}
