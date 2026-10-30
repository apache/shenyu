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

package org.apache.shenyu.plugin.sync.data.websocket;

import java.net.URI;
import java.util.List;
import java.util.Locale;
import java.util.Objects;

/**
 * Decides whether an advertised cluster master may receive the synchronization credential.
 *
 * <p>A destination is trusted only when its origin is one of the configured seeds, an already
 * connected client, or an explicitly configured trusted master origin. A secure {@code wss}
 * connection never follows a plaintext {@code ws} advertisement unless the operator explicitly
 * enables insecure redirects. This check runs before a client, and therefore its handshake
 * headers, is constructed.</p>
 */
final class MasterTargetPolicy {

    private static final String WS = "ws";

    private static final String WSS = "wss";

    private static final int WS_DEFAULT_PORT = 80;

    private static final int WSS_DEFAULT_PORT = 443;

    private MasterTargetPolicy() {
    }

    /**
     * Whether the advertised master may receive the synchronization credential.
     *
     * @param advertised the advertised master URI
     * @param current the URI of the connection that advertised it
     * @param connected the URIs of the currently connected clients
     * @param seedUrls the configured seed URLs
     * @param trustedEntries additional trusted host entries
     * @param allowInsecure whether a wss connection may follow a ws advertisement
     * @return true when the destination and transport are allowed
     */
    static boolean isTrustedMaster(final URI advertised, final URI current, final List<URI> connected,
                                   final List<String> seedUrls, final List<String> trustedEntries,
                                   final boolean allowInsecure) {
        if (!isWebSocket(advertised) || !transportAllowed(current, advertised, allowInsecure)) {
            return false;
        }
        final Origin target = originOf(advertised);
        if (Objects.isNull(target)) {
            return false;
        }
        if (target.equals(originOf(current))) {
            return true;
        }
        if (Objects.nonNull(connected)
                && connected.stream().filter(Objects::nonNull).anyMatch(uri -> target.equals(originOf(uri)))) {
            return true;
        }
        if (Objects.nonNull(seedUrls)
                && seedUrls.stream().map(MasterTargetPolicy::parseSeed).filter(Objects::nonNull)
                .anyMatch(uri -> target.equals(originOf(uri)))) {
            return true;
        }
        return Objects.nonNull(trustedEntries) && trustedEntries.stream().anyMatch(entry -> matchesEntry(entry, target));
    }

    private static boolean isWebSocket(final URI uri) {
        return Objects.nonNull(uri)
                && (WS.equalsIgnoreCase(uri.getScheme()) || WSS.equalsIgnoreCase(uri.getScheme()))
                && Objects.nonNull(uri.getHost());
    }

    private static boolean transportAllowed(final URI current, final URI advertised, final boolean allowInsecure) {
        if (Objects.isNull(current)) {
            return false;
        }
        return !(WSS.equalsIgnoreCase(current.getScheme()) && WS.equalsIgnoreCase(advertised.getScheme()))
                || allowInsecure;
    }

    private static Origin originOf(final URI uri) {
        if (Objects.isNull(uri) || Objects.isNull(uri.getHost())) {
            return null;
        }
        int port = uri.getPort();
        if (port < 0) {
            port = WSS.equalsIgnoreCase(uri.getScheme()) ? WSS_DEFAULT_PORT : WS_DEFAULT_PORT;
        }
        return new Origin(uri.getHost(), port);
    }

    private static URI parseSeed(final String seed) {
        if (Objects.isNull(seed) || seed.isBlank()) {
            return null;
        }
        try {
            return URI.create(seed.contains("://") ? seed : WS + "://" + seed);
        } catch (IllegalArgumentException ex) {
            return null;
        }
    }

    private static boolean matchesEntry(final String entry, final Origin target) {
        if (Objects.isNull(entry) || entry.isBlank()) {
            return false;
        }
        final String value = entry.trim();
        if (value.contains("://")) {
            final URI uri = parseSeed(value);
            return Objects.nonNull(uri) && target.equals(originOf(uri));
        }
        if (value.startsWith("[")) {
            final int end = value.indexOf(']');
            if (end <= 0) {
                return false;
            }
            final String host = value.substring(1, end);
            final boolean hasPort = end + 1 < value.length() && value.charAt(end + 1) == ':';
            return host.equalsIgnoreCase(target.getHost())
                    && (!hasPort || parsePort(value.substring(end + 2)) == target.getPort());
        }
        final int firstColon = value.indexOf(':');
        final int lastColon = value.lastIndexOf(':');
        if (firstColon > 0 && firstColon == lastColon) {
            return value.substring(0, firstColon).equalsIgnoreCase(target.getHost())
                    && parsePort(value.substring(firstColon + 1)) == target.getPort();
        }
        return value.equalsIgnoreCase(target.getHost());
    }

    private static int parsePort(final String value) {
        try {
            return Integer.parseInt(value);
        } catch (NumberFormatException ex) {
            return -1;
        }
    }

    /**
     * A host and effective port; the scheme is checked separately.
     */
    private static final class Origin {

        private final String host;

        private final int port;

        private Origin(final String host, final int port) {
            this.host = normalizeHost(host);
            this.port = port;
        }

        private static String normalizeHost(final String host) {
            final String normalized = host.toLowerCase(Locale.ROOT);
            if (normalized.length() > 1 && normalized.charAt(0) == '[' && normalized.charAt(normalized.length() - 1) == ']') {
                return normalized.substring(1, normalized.length() - 1);
            }
            return normalized;
        }

        private String getHost() {
            return host;
        }

        private int getPort() {
            return port;
        }

        @Override
        public boolean equals(final Object other) {
            if (this == other) {
                return true;
            }
            if (!(other instanceof Origin)) {
                return false;
            }
            final Origin origin = (Origin) other;
            return port == origin.port && host.equals(origin.host);
        }

        @Override
        public int hashCode() {
            return Objects.hash(host, port);
        }
    }
}
