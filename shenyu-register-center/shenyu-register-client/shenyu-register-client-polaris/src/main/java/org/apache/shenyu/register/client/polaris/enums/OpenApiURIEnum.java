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

package org.apache.shenyu.register.client.polaris.enums;

import org.apache.hc.core5.http.Method;

public enum OpenApiURIEnum {

    /**
     * release config file.
     */
    URI_RELEASE_CONFIG("/config/v1/configfiles/release", Method.POST),

    /**
     * create config file.
     */
    URI_CREATE_CONFIG("/config/v1/configfiles", Method.POST),


    /**
     * update config file.
     */
    URI_UPDATE_CONFIG("/config/v1/configfiles", Method.PUT),

    /**
     * get config file.
     */
    URI_GET_CONFIG("/config/v1/configfiles", Method.GET);

    private final String uri;
    private final Method method;

    OpenApiURIEnum(final String uri, final Method method) {
        this.uri = uri;
        this.method = method;
    }

    /**
     * get uri.
     * @return String
     */
    public String getUri() {
        return uri;
    }

    /**
     * get method.
     * @return Method
     */
    public Method getMethod() {
        return method;
    }
}
