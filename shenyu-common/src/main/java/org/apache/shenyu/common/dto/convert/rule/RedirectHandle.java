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

package org.apache.shenyu.common.dto.convert.rule;

import java.util.Objects;

/**
 * This is redirect plugin handle.
 */
public class RedirectHandle {
    
    /**
     * http status code.
     */
    private Integer httpStatusCode = 302;
    
    /**
     * redirect url.
     */
    private String redirectURI;
    
    /**
     * Gets http status code.
     *
     * @return the http status code
     */
    public Integer getHttpStatusCode() {
        return httpStatusCode;
    }
    
    /**
     * Sets http status code.
     *
     * @param httpStatusCode the http status code
     */
    public void setHttpStatusCode(final Integer httpStatusCode) {
        this.httpStatusCode = httpStatusCode;
    }
    
    /**
     * get redirectURI.
     *
     * @return redirectURI redirect uri
     */
    public String getRedirectURI() {
        return redirectURI;
    }
    
    /**
     * set redirectURI.
     *
     * @param redirectURI redirectURI
     */
    public void setRedirectURI(final String redirectURI) {
        this.redirectURI = redirectURI;
    }
    
    @Override
    public String toString() {
        return "RedirectHandle{" + "httpStatusCode=" + httpStatusCode + ", redirectURI='" + redirectURI + '\'' + '}';
    }
    
    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;  
        }
        final RedirectHandle that = (RedirectHandle) o;
        return Objects.equals(httpStatusCode, that.httpStatusCode) && Objects.equals(redirectURI, that.redirectURI);
    }
    
    @Override
    public int hashCode() {
        return Objects.hash(httpStatusCode, redirectURI);
    }
}
