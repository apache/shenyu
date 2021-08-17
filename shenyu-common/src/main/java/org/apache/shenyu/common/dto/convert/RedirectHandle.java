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

package org.apache.shenyu.common.dto.convert;

import java.util.Objects;

/**
 * This is redirect plugin handle.
 */
public class RedirectHandle {
    /**
     * redirect url.
     */
    private String redirectURI;

    /**
     * get redirectURI.
     *
     * @return redirectURI
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
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        RedirectHandle that = (RedirectHandle) o;
        return Objects.equals(redirectURI, that.redirectURI);
    }

    @Override
    public int hashCode() {
        return Objects.hash(redirectURI);
    }

    @Override
    public String toString() {
        return "RedirectHandle{"
                + "redirectURI='"
                + redirectURI
                + '\''
                + '}';
    }
}
