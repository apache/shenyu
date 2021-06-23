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

package org.apache.shenyu.common.utils;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.Constants;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;

/**
 * The type Uri utils.
 */
public class UpstreamCheckUtils {

    private static final String HTTP = "http";

    private static final String HTTPS = "https";

    private static final int DEFAULT_TIMEOUT = 3000;

    /**
     * Check url boolean.
     *
     * @param url the url
     * @return the boolean
     */
    public static boolean checkUrl(final String url) {
        return checkUrl(url, DEFAULT_TIMEOUT);
    }

    /**
     * Check url boolean.
     *
     * @param url     the url
     * @param timeout timeout
     * @return the boolean
     */
    public static boolean checkUrl(final String url, final int timeout) {
        if (StringUtils.isBlank(url)) {
            return false;
        }
        String[] hostPort;
        if (url.startsWith(HTTP)) {
            final String[] http = StringUtils.split(url, "\\/\\/");
            hostPort = StringUtils.split(http[1], Constants.COLONS);
        } else {
            hostPort = StringUtils.split(url, Constants.COLONS);
        }
        final boolean isHttps = url.startsWith(HTTPS);
        final int port = hostPort.length > 1 ? Integer.parseInt(hostPort[1]) : isHttps ? 443 : 80;
        return isHostConnector(hostPort[0], port, timeout);
    }

    private static boolean isHostConnector(final String host, final int port, final int timeout) {
        try (Socket socket = new Socket()) {
            socket.connect(new InetSocketAddress(host, port), timeout);
        } catch (IOException e) {
            return false;
        }
        return true;
    }
}
