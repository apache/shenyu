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

package org.apache.shenyu.register.common.protocol;

import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Locale;
import java.util.Objects;

/**
 * Canonicalizes the address identity used by registration fencing.
 *
 * <p>Only IPv6 literals are passed to the JDK parser. DNS names are never
 * resolved while constructing an owner key.</p>
 */
public final class RegistrationFencingAddress {

    private RegistrationFencingAddress() {
    }

    /**
     * Canonicalize a host without resolving DNS names.
     *
     * @param host host value
     * @return canonical host
     */
    public static String canonicalHost(final String host) {
        if (Objects.isNull(host) || host.isBlank() || !host.trim().equals(host)) {
            throw new IllegalArgumentException("host must not be blank or padded");
        }
        if (host.indexOf(':') >= 0) {
            if (!host.matches("[0-9A-Fa-f:.]+")) {
                throw new IllegalArgumentException("invalid IPv6 literal");
            }
            try {
                InetAddress address = InetAddress.getByName(host);
                if (!(address instanceof Inet6Address)) {
                    throw new IllegalArgumentException("invalid IPv6 literal");
                }
                return canonicalIpv6(address.getAddress());
            } catch (UnknownHostException exception) {
                throw new IllegalArgumentException("invalid IPv6 literal", exception);
            }
        }
        if (host.matches("[0-9.]+")) {
            validateIpv4(host);
            return host;
        }
        if (!host.matches("[A-Za-z0-9.-]+")) {
            throw new IllegalArgumentException("invalid host");
        }
        for (String label : host.split("\\.", -1)) {
            if (label.isEmpty() || label.startsWith("-") || label.endsWith("-") || label.length() > 63) {
                throw new IllegalArgumentException("invalid DNS host");
            }
        }
        return host.toLowerCase(Locale.ROOT);
    }

    /**
     * Canonicalize a protocol.
     *
     * @param protocol protocol
     * @return canonical protocol
     */
    public static String canonicalProtocol(final String protocol) {
        if (Objects.isNull(protocol) || protocol.isBlank() || !protocol.trim().equals(protocol)) {
            throw new IllegalArgumentException("protocol must not be blank or padded");
        }
        return protocol.toLowerCase(Locale.ROOT);
    }

    /**
     * Canonicalize an existing host:port value.
     *
     * @param endpoint endpoint
     * @return canonical endpoint
     */
    public static String canonicalEndpoint(final String endpoint) {
        if (Objects.isNull(endpoint) || endpoint.isBlank()) {
            throw new IllegalArgumentException("endpoint must not be blank");
        }
        String value = endpoint.trim();
        int separator = value.lastIndexOf(':');
        if (separator <= 0 || separator == value.length() - 1) {
            throw new IllegalArgumentException("invalid endpoint");
        }
        String host = value.substring(0, separator);
        if (host.startsWith("[") && host.endsWith("]")) {
            host = host.substring(1, host.length() - 1);
        }
        int port;
        try {
            port = Integer.parseInt(value.substring(separator + 1));
        } catch (NumberFormatException exception) {
            throw new IllegalArgumentException("invalid endpoint", exception);
        }
        if (port < 1 || port > 65535) {
            throw new IllegalArgumentException("invalid endpoint port");
        }
        return canonicalHost(host) + ":" + port;
    }

    private static void validateIpv4(final String host) {
        String[] octets = host.split("\\.", -1);
        if (octets.length != 4) {
            throw new IllegalArgumentException("invalid IPv4 literal");
        }
        for (String octet : octets) {
            if (octet.isEmpty() || octet.length() > 1 && octet.startsWith("0")) {
                throw new IllegalArgumentException("invalid IPv4 literal");
            }
            int value;
            try {
                value = Integer.parseInt(octet);
            } catch (NumberFormatException exception) {
                throw new IllegalArgumentException("invalid IPv4 literal", exception);
            }
            if (value > 255) {
                throw new IllegalArgumentException("invalid IPv4 literal");
            }
        }
    }

    private static String canonicalIpv6(final byte[] address) {
        int[] words = new int[8];
        for (int i = 0; i < words.length; i++) {
            words[i] = (Byte.toUnsignedInt(address[i * 2]) << 8) | Byte.toUnsignedInt(address[i * 2 + 1]);
        }
        int bestStart = -1;
        int bestLength = 1;
        for (int i = 0; i < words.length;) {
            if (words[i] != 0) {
                i++;
                continue;
            }
            int end = i;
            while (end < words.length && words[end] == 0) {
                end++;
            }
            if (end - i > bestLength) {
                bestStart = i;
                bestLength = end - i;
            }
            i = end;
        }
        StringBuilder result = new StringBuilder();
        for (int i = 0; i < words.length;) {
            if (i == bestStart) {
                result.append("::");
                i += bestLength;
            } else {
                if (result.length() > 0 && result.charAt(result.length() - 1) != ':') {
                    result.append(':');
                }
                result.append(Integer.toHexString(words[i++]));
            }
        }
        return result.toString();
    }
}
