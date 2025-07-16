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

import java.net.InetAddress;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

/**
 * URL security utilities for preventing SSRF and other URL-based attacks.
 */
public final class UrlSecurityUtils {

    /**
     * Private constructor to prevent instantiation.
     */
    private UrlSecurityUtils() {
    }

    /**
     * Validate URL to prevent SSRF attacks.
     *
     * @param url the URL to validate
     * @throws IllegalArgumentException if the URL is not safe for external requests
     */
    public static void validateUrlForSSRF(final String url) {
        if (Objects.isNull(url) || url.trim().isEmpty()) {
            throw new IllegalArgumentException("URL cannot be empty");
        }

        try {
            URL parsedUrl = new URL(url);
            String protocol = parsedUrl.getProtocol();

            // Only allow HTTP and HTTPS protocols
            if (!"http".equals(protocol) && !"https".equals(protocol)) {
                throw new IllegalArgumentException("Only HTTP and HTTPS protocols are allowed");
            }

            // Validate host for SSRF protection
            validateHostForSSRF(parsedUrl.getHost(), parsedUrl.getPort());

        } catch (MalformedURLException e) {
            throw new IllegalArgumentException("Invalid URL format: " + e.getMessage());
        }
    }

    /**
     * Validate host to prevent SSRF attacks.
     *
     * @param host the host to validate
     * @param port the port to validate
     * @throws IllegalArgumentException if the host is not allowed
     */
    public static void validateHostForSSRF(final String host, final int port) {
        if (Objects.isNull(host) || host.trim().isEmpty()) {
            throw new IllegalArgumentException("Host cannot be empty");
        }

        String normalizedHost = host.toLowerCase().trim();

        // Check for localhost variations
        if (isLocalhost(normalizedHost)) {
            throw new IllegalArgumentException("Access to localhost is not allowed");
        }

        // Check for private IP addresses
        if (isPrivateOrInternalIP(normalizedHost)) {
            throw new IllegalArgumentException("Access to private or internal IP addresses is not allowed");
        }

        // Check for sensitive ports
        if (isSensitivePort(port)) {
            throw new IllegalArgumentException("Access to sensitive ports is not allowed");
        }

        // Additional validation for DNS resolution
        try {
            InetAddress[] addresses = InetAddress.getAllByName(normalizedHost);
            for (InetAddress address : addresses) {
                if (address.isLoopbackAddress() || address.isLinkLocalAddress()
                        || address.isSiteLocalAddress() || address.isAnyLocalAddress()) {
                    throw new IllegalArgumentException("Resolved IP address is not allowed: " + address.getHostAddress());
                }

                // Check resolved IP against private ranges
                if (isPrivateIPAddress(address.getHostAddress())) {
                    throw new IllegalArgumentException("Resolved IP address is private: " + address.getHostAddress());
                }
            }
        } catch (UnknownHostException e) {
            throw new IllegalArgumentException("Cannot resolve host: " + host);
        }
    }

    /**
     * Check if the host is localhost or localhost variations.
     *
     * @param host the host to check
     * @return true if the host is localhost
     */
    private static boolean isLocalhost(final String host) {
        Set<String> localhostVariations = new HashSet<>(Arrays.asList(
                "localhost", "127.0.0.1", "::1", "0.0.0.0", "0000:0000:0000:0000:0000:0000:0000:0001"
        ));
        return localhostVariations.contains(host);
    }

    /**
     * Check if the host is a private or internal IP address.
     *
     * @param host the host to check
     * @return true if the host is private or internal
     */
    private static boolean isPrivateOrInternalIP(final String host) {
        // Check for IPv4 private ranges
        if (host.matches("^10\\..*")
                || host.matches("^172\\.(1[6-9]|2[0-9]|3[0-1])\\..*")
                || host.matches("^192\\.168\\..*")) {
            return true;
        }

        // Check for IPv6 private ranges
        if (host.startsWith("fc") || host.startsWith("fd")
                || host.startsWith("fe80") || "::1".equals(host)) {
            return true;
        }

        // Check for other internal addresses
        if (host.matches("^169\\.254\\..*")
                || host.matches("^224\\..*")
                || host.matches("^255\\..*")) {
            return true;
        }

        return false;
    }

    /**
     * Check if the resolved IP address is private.
     *
     * @param ip the IP address to check
     * @return true if the IP is private
     */
    private static boolean isPrivateIPAddress(final String ip) {
        try {
            InetAddress address = InetAddress.getByName(ip);
            return address.isSiteLocalAddress() || address.isLoopbackAddress()
                    || address.isLinkLocalAddress() || address.isAnyLocalAddress();
        } catch (UnknownHostException e) {
            return false;
        }
    }

    /**
     * Check if the port is sensitive (commonly used for internal services).
     *
     * @param port the port to check
     * @return true if the port is sensitive
     */
    private static boolean isSensitivePort(final int port) {
        if (port == -1) {
            // Default port, will be 80 or 443
            return false;
        }

        // Common sensitive ports
        Set<Integer> sensitivePorts = new HashSet<>(Arrays.asList(
                /* SSH and remote access */
                22, 23, 3389,
                /* Email services */
                25,
                /* DNS */
                53,
                /* Windows services */
                135, 139, 445,
                /* Database services */
                5432, 3306, 1433,
                /* Cache services */
                6379, 11211,
                /* Search and storage */
                5984, 9200,
                /* Middleware */
                2181, 9092, 9093,
                /* Common internal web services */
                8080, 8081, 9090, 9091,
                /* Container services */
                2375, 2376,
                /* Message queue */
                25672, 5672, 15672,
                /* Other services */
                4369
        ));

        return sensitivePorts.contains(port);
    }
} 