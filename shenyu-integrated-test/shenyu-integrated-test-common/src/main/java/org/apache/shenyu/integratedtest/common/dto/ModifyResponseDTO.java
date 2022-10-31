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

public class ModifyResponseDTO {

    private UserDTO body;

    private String cookie;

    private String header;

    private String parameter;

    public ModifyResponseDTO() {
    }

    public ModifyResponseDTO(final UserDTO body, final String cookie, final String header, final String parameter) {
        this.body = body;
        this.cookie = cookie;
        this.header = header;
        this.parameter = parameter;
    }

    /**
     * get body.
     *
     * @return UserDTO
     */
    public UserDTO getBody() {
        return body;
    }

    /**
     * set body.
     *
     * @param body body
     */
    public void setBody(final UserDTO body) {
        this.body = body;
    }

    /**
     * get cookie.
     *
     * @return the cookie
     */
    public String getCookie() {
        return cookie;
    }

    /**
     * set cookie.
     *
     * @param cookie the cookie
     */
    public void setCookie(final String cookie) {
        this.cookie = cookie;
    }

    /**
     * get header.
     *
     * @return the header
     */
    public String getHeader() {
        return header;
    }

    /**
     * set header.
     *
     * @param header the header
     */
    public void setHeader(final String header) {
        this.header = header;
    }

    /**
     * get parameter.
     *
     * @return the parameter
     */
    public String getParameter() {
        return parameter;
    }

    /**
     * set parameter.
     *
     * @param parameter the parameter
     */
    public void setParameter(final String parameter) {
        this.parameter = parameter;
    }
}
