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

package org.dromara.soul.common.http;

import lombok.Getter;

import java.net.MalformedURLException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * URL .
 * HTTP URL 处理.
 * ctime: 2019/10/18 下午4:47
 *
 * @author sixh
 */
@Getter
public class URL {
    private String protocol;
    private String host;
    private Integer port;
    private String path;
    private Map<String, String> parameters;
    private String full;

    public static URL parse(String url) {
        URL newUrl = new URL();
        try {
            newUrl.full = url;
            java.net.URL u = new java.net.URL(url);
            newUrl.protocol = u.getProtocol();
            newUrl.host = u.getHost();
            newUrl.port = u.getPort();
            newUrl.path = u.getPath();
            String query = u.getQuery();
            //Formatted field.
            if (query != null && query.trim().length() > 0) {
                String[] split = query.split("&");
                Map<String, String> params = new HashMap<>(split.length);
                Arrays.stream(split)
                        .filter(e -> e.length() > 0)
                        .forEach(e -> {
                            int partIndex;
                            if ((partIndex = e.indexOf('=')) > 0) {
                                String left = e.substring(0, partIndex);
                                String right = e.substring(partIndex+1);
                                params.put(left, right);
                            } else {
                                params.put(e, "");
                            }
                        });
                newUrl.parameters = params;
            }
        } catch (MalformedURLException e) {
            throw new IllegalArgumentException("invalid url");
        }
        return newUrl;
    }

    @Override
    public String toString() {
        return full;
    }
}
