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

package org.dromara.soul.common.utils;

import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.common.constant.Constants;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.regex.Pattern;

/**
 * The type Uri utils.
 *
 * @author xiaoyu(Myth)
 */
public class UpstreamCheckUtils {

    private static final Pattern PATTERN = Pattern
            .compile("(http://|https://)?(?:(?:[0,1]?\\d?\\d|2[0-4]\\d|25[0-5])\\.){3}(?:[0,1]?\\d?\\d|2[0-4]\\d|25[0-5])(:\\d{0,5})?");

    private static final String HTTP = "http";

    private static final String HTTPS = "https";

    /**
     * Check url boolean.
     *
     * @param url the url
     * @return the boolean
     */
    public static boolean checkUrl(final String url) {
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
        if (checkIP(hostPort[0]) || isHttps) {
            return isHostConnector(hostPort[0], port);
        } else {
            return isHostReachable(hostPort[0]);
        }
    }

    private static boolean checkIP(final String url) {
        return PATTERN.matcher(url).matches();
    }

    private static boolean isHostConnector(final String host, final int port) {
        try (Socket socket = new Socket()) {
            socket.connect(new InetSocketAddress(host, port));
        } catch (IOException e) {
            return false;
        }
        return true;
    }

    private static boolean isHostReachable(final String host) {
        try {
            return InetAddress.getByName(host).isReachable(1000);
        } catch (IOException ignored) {
        }
        return false;
    }

}
