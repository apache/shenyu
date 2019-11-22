/*
 *     Licensed to the Apache Software Foundation (ASF) under one or more
 *     contributor license agreements.See the NOTICE file distributed with
 *     this work for additional information regarding copyright ownership.
 *     The ASF licenses this file to You under the Apache License, Version 2.0
 *     (the "License"); you may not use this file except in compliance with
 *     the License.You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *     Unless required by applicable law or agreed to in writing, software
 *     distributed under the License is distributed on an "AS IS" BASIS,
 *     WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *     See the License for the specific language governing permissions and
 *     limitations under the License.
 */

package org.dromara.soul.register;


import org.dromara.soul.common.http.URL;

/**
 * URLAdapter
 *
 * @author sixh
 */
public class UrlAdapter {

    /**
     * Parse url.
     *
     * @param url the url
     * @return the url
     */
    public static URL parse(com.alibaba.dubbo.common.URL url) {
        return URL.parse(url.toString());
    }

    /**
     * Parse alibaba com . alibaba . dubbo . common . url.
     *
     * @param url the url
     * @return the com . alibaba . dubbo . common . url
     */
    public static com.alibaba.dubbo.common.URL parseAlibaba(URL url) {
        return com.alibaba.dubbo.common.URL.valueOf(url.fullString());
    }

    /**
     * Parse url.
     *
     * @param url the url
     * @return the url
     */
    public static URL parse(org.apache.dubbo.common.URL url) {
        return URL.parse(url.toString());
    }
}
