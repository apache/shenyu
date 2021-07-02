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

import java.net.InetAddress;
import java.net.NetworkInterface;
import java.util.Enumeration;
import java.util.regex.Pattern;

/**
 * The type Ip utils.
 */
public final class IpUtils {

    /**
     * ip pattern.
     */
    private static final Pattern IP_PATTERN = Pattern.compile("^((25[0-5]|2[0-4]\\d|[01]?\\d\\d?)($|(?!\\.$)\\.)){4}$");

    private IpUtils() {
    }

    /**
     * Gets host.
     *
     * @return the host
     */
    public static String getHost() {
        return getHost(null);
    }

    /**
     * Gets host.
     *
     * @param filterHost host filterHost str
     * @return the host
     */
    public static String getHost(final String filterHost) {
        String firstHostIp = null;
        String hostIp = null;
        String pattern = filterHost;
        // I think this function is only support ipv4.
        if (filterHost != null && !filterHost.contains("*") && !isCompleteHost(filterHost)) {
            pattern = filterHost + "*";
        }
        try {
            Enumeration<?> networkInterfaces = NetworkInterface.getNetworkInterfaces();
            while (networkInterfaces.hasMoreElements()) {
                NetworkInterface network = (NetworkInterface) networkInterfaces.nextElement();
                Enumeration<?> addresses = network.getInetAddresses();
                while (addresses.hasMoreElements()) {
                    InetAddress inetAddress = (InetAddress) addresses.nextElement();
                    String hostAddress = inetAddress.getHostAddress();
                    if (hostAddress.contains(".") && !inetAddress.isLoopbackAddress()) {
                        if (firstHostIp == null) {
                            firstHostIp = hostAddress;
                            if (filterHost == null) {
                                break;
                            }
                        }
                        if (pattern != null && ipMatch(hostAddress, pattern)) {
                            hostIp = hostAddress;
                        }
                    }
                }
            }

            if (hostIp == null && firstHostIp != null) {
                hostIp = firstHostIp;
            }

            if (hostIp == null) {
                hostIp = InetAddress.getLocalHost().getHostAddress();
            }
        } catch (Exception ignore) {
            hostIp = "127.0.0.1";
        }
        return hostIp;
    }

    /**
     * Judge whether 'host'"' is complete.
     *
     * @param host host ip
     * @return boolean
     */
    public static boolean isCompleteHost(final String host) {
        if (host == null) {
            return false;
        }
        return IP_PATTERN.matcher(host).matches();
    }

    /**
     * do ip match.
     *
     * @param ip network ip
     * @param pattern match pattern
     * @return boolean
     */
    private static boolean ipMatch(final String ip, final String pattern) {
        int m = ip.length();
        int n = pattern.length();
        boolean[][] dp = new boolean[m + 1][n + 1];
        dp[0][0] = true;
        for (int i = 1; i <= n; ++i) {
            if (pattern.charAt(i - 1) == '*') {
                dp[0][i] = true;
            } else {
                break;
            }
        }
        for (int i = 1; i <= m; ++i) {
            for (int j = 1; j <= n; ++j) {
                if (pattern.charAt(j - 1) == '*') {
                    dp[i][j] = dp[i][j - 1] || dp[i - 1][j];
                } else if (pattern.charAt(j - 1) == '?' || ip.charAt(i - 1) == pattern.charAt(j - 1)) {
                    dp[i][j] = dp[i - 1][j - 1];
                }
            }
        }
        return dp[m][n];
    }
}
