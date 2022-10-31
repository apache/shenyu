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

package org.apache.shenyu.plugin.sync.data.websocket.config;

import java.util.Objects;

public class WebsocketConfig {
    
    /**
     * if have more shenyu admin url,please config like this.
     * 127.0.0.1:8888,127.0.0.1:8889
     */
    private String urls;

    /**
     * allowOrigin.
     */
    private String allowOrigin;

    /**
     * get urls.
     *
     * @return urls
     */
    public String getUrls() {
        return urls;
    }

    /**
     * set urls.
     *
     * @param urls urls
     */
    public void setUrls(final String urls) {
        this.urls = urls;
    }

    /**
     * get allowOrigin.
     * @return allowOrigin
     */
    public String getAllowOrigin() {
        return allowOrigin;
    }

    /**
     * set allowOrigin.
     * @param allowOrigin allowOrigin
     */
    public void setAllowOrigin(final String allowOrigin) {
        this.allowOrigin = allowOrigin;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        WebsocketConfig that = (WebsocketConfig) o;
        return Objects.equals(urls, that.urls) && Objects.equals(allowOrigin, that.allowOrigin);
    }

    @Override
    public int hashCode() {
        return Objects.hash(urls, allowOrigin);
    }

    @Override
    public String toString() {
        return "WebsocketConfig{"
                + "urls='"
                + urls
                + ", allowOrigin='"
                + allowOrigin
                + '}';
    }
}
