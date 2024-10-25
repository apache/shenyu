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

package org.apache.shenyu.admin.scale.config;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

import java.util.Map;

@Component
@ConfigurationProperties(prefix = "shenyu.k8s.prometheus")
public class PrometheusProperties {

    private String url;

    private Map<String, String> queries;

    /**
     * getUrl.
     *
     * @return String
     */
    public String getUrl() {
        return url;
    }

    /**
     * setUrl.
     *
     * @param url url
     */
    public void setUrl(final String url) {
        this.url = url;
    }

    /**
     * getQueries.
     *
     * @return Map
     */
    public Map<String, String> getQueries() {
        return queries;
    }

    /**
     * setQueries.
     *
     * @param queries queries
     */
    public void setQueries(final Map<String, String> queries) {
        this.queries = queries;
    }
}
