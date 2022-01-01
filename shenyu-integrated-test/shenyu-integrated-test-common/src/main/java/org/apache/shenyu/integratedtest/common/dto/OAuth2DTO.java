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

package org.apache.shenyu.integratedtest.common.dto;

public class OAuth2DTO {

    private String token;

    /**
     * Get token.
     *
     * @return token
     */
    public String getToken() {
        return token;
    }

    /**
     * Set token.
     *
     * @param token token
     */
    public void setToken(final String token) {
        this.token = token;
    }

    @Override
    public String toString() {
        return "OAuth2DTO{" + "token='" + token + '\'' + '}';
    }

}
