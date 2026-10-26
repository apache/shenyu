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

package org.apache.shenyu.admin.utils;

import okhttp3.Dns;
import okhttp3.HttpUrl;
import org.apache.shenyu.admin.config.properties.SwaggerImportProperties;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.List;
import java.util.Objects;

/**
 * URL security utilities for preventing SSRF and other URL-based attacks.
 */
public final class UrlSecurityUtils {

    private UrlSecurityUtils() {
    }

    /**
     * Validate a URL using the default Swagger import policy.
     * @param url URL to validate
     * @throws IllegalArgumentException if the URL is not safe for external requests
     */
    public static void validateUrlForSSRF(final String url) {
        validatedDns(url, new SwaggerImportProperties(), Dns.SYSTEM);
    }

    /**
     * Validate a host using the default Swagger import policy.
     * @param host host to validate
     * @param port port to validate, or -1 for the default HTTP port
     * @throws IllegalArgumentException if the host is not allowed
     */
    public static void validateHostForSSRF(final String host, final int port) {
        if (Objects.isNull(host) || host.trim().isEmpty()) {
            throw new IllegalArgumentException("Host cannot be empty");
        }
        validateUrlForSSRF(new HttpUrl.Builder().scheme("http").host(host.trim())
                .port(port == -1 ? 80 : port).build().toString());
    }

    /**
     * Resolve and validate every destination once, then pin that result for request execution.
     * The returned DNS rejects other hostnames; callers must also disable redirects and proxies
     * and isolate their connection pool so a previous connection cannot bypass this resolution.
     * @param url URL to validate, parsed using the same parser as OkHttp requests
     * @param properties allowed and denied port policy
     * @param resolver resolver used for this request
     * @return DNS restricted to an immutable copy of the validated addresses
     * @throws IllegalArgumentException if the URL, port or any resolved address is not allowed
     */
    public static Dns validatedDns(final String url, final SwaggerImportProperties properties, final Dns resolver) {
        if (Objects.isNull(url) || url.trim().isEmpty()) {
            throw new IllegalArgumentException("URL cannot be empty");
        }
        HttpUrl parsedUrl = HttpUrl.parse(url);
        if (Objects.isNull(parsedUrl)) {
            throw new IllegalArgumentException("Invalid HTTP or HTTPS URL");
        }
        int port = parsedUrl.port();
        if (properties.getDeniedPorts().contains(port)
                || !properties.getAllowedPorts().isEmpty() && !properties.getAllowedPorts().contains(port)) {
            throw new IllegalArgumentException("Access to this port is not allowed");
        }
        List<InetAddress> addresses;
        try {
            addresses = resolver.lookup(parsedUrl.host());
        } catch (UnknownHostException e) {
            throw new IllegalArgumentException("Cannot resolve host: " + parsedUrl.host(), e);
        }
        if (Objects.isNull(addresses) || addresses.isEmpty() || addresses.stream().anyMatch(UrlSecurityUtils::isInternalAddress)) {
            throw new IllegalArgumentException("Resolved IP address is private or internal");
        }
        List<InetAddress> validatedAddresses = List.copyOf(addresses);
        return hostname -> {
            if (!parsedUrl.host().equals(hostname)) {
                throw new UnknownHostException("Host differs from validated request");
            }
            return validatedAddresses;
        };
    }

    private static boolean isInternalAddress(final InetAddress address) {
        if (Objects.isNull(address) || address.isLoopbackAddress() || address.isLinkLocalAddress()
                || address.isSiteLocalAddress() || address.isAnyLocalAddress() || address.isMulticastAddress()) {
            return true;
        }
        byte[] bytes = address.getAddress();
        int first = bytes[0] & 0xff;
        if (bytes.length == 4) {
            return isInternalIpv4Address(bytes, 0);
        }
        if (isIpv4MappedAddress(bytes)) {
            return isInternalIpv4Address(bytes, 12);
        }
        // Java does not classify IPv6 unique-local addresses as site-local.
        return (first & 0xfe) == 0xfc;
    }

    private static boolean isIpv4MappedAddress(final byte[] bytes) {
        if (bytes.length != 16 || bytes[10] != (byte) 0xff || bytes[11] != (byte) 0xff) {
            return false;
        }
        for (int index = 0; index < 10; index++) {
            if (bytes[index] != 0) {
                return false;
            }
        }
        return true;
    }

    private static boolean isInternalIpv4Address(final byte[] bytes, final int offset) {
        int first = bytes[offset] & 0xff;
        int second = bytes[offset + 1] & 0xff;
        return first == 0 || first == 10 || first == 127 || first >= 224
                || first == 169 && second == 254 || first == 172 && second >= 16 && second <= 31
                || first == 192 && second == 168 || first == 100 && second >= 64 && second <= 127;
    }
}
