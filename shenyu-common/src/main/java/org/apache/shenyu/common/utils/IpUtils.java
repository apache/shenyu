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

/**
 * The type Ip utils.
 */
public final class IpUtils {
    private IpUtils() {
    }

    /**
     * Gets host.
     *
     * @return the host
     */
    public static String getHost() {
        String hostIp = null;
        try {
            Enumeration<?> networkInterfaces = NetworkInterface.getNetworkInterfaces();
            while (networkInterfaces.hasMoreElements()) {
                NetworkInterface network = (NetworkInterface) networkInterfaces.nextElement();
                Enumeration<?> addresses = network.getInetAddresses();
                while (addresses.hasMoreElements()) {
                    InetAddress inetAddress = (InetAddress) addresses.nextElement();
                    String hostAddress = inetAddress.getHostAddress();
                    if (hostAddress.contains(".") && !inetAddress.isLoopbackAddress()) {
                        hostIp = hostAddress;
                        break;
                    }
                }
            }
            if (hostIp == null) {
                hostIp = InetAddress.getLocalHost().getHostAddress();
            }
        } catch (Exception ignore) {
            hostIp = "127.0.0.1";
        }
        return hostIp;
    }
}
