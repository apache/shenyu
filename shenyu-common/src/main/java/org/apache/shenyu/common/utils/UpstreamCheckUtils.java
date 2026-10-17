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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.URI;

/**
 * The type Uri utils.
 */
public class UpstreamCheckUtils {

    private static final String HTTP = "http://";

    private static final String HTTPS = "https://";

    private static final int DEFAULT_TIMEOUT = 3000;

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(UpstreamCheckUtils.class);

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
        final String host;
        final int port;
        if (url.startsWith(HTTP) || url.startsWith(HTTPS)) {
            try {
                URI uri = new URI(url);
                host = uri.getHost();
                port = uri.getPort() == -1 ? url.startsWith(HTTPS) ? 443 : 80 : uri.getPort();
            } catch (Exception e) {
                LOG.error("Invalid URL: {}", url, e);
                return false;
            }
        } else {
            try {
                String[] parts = IpUtils.parseHostPort(url);
                host = parts[0];
                port = Integer.parseInt(parts[1]);
            } catch (Exception e) {
                LOG.error("Invalid URL: {}", url, e);
                return false;
            }
        }
        return isHostConnector(host.trim(), port, timeout);
    }

    private static boolean isHostConnector(final String host, final int port, final int timeout) {
        try (Socket socket = new Socket()) {
            socket.connect(new InetSocketAddress(host, port), timeout);
        } catch (Exception e) {
            LOG.error("socket connect is error. host:{} port:{} timeout:{}", host, port, timeout, e);
            return false;
        }
        return true;
    }
}
