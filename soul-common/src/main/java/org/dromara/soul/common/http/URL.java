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
import org.dromara.soul.common.exception.SoulException;
import org.dromara.soul.common.utils.StringUtils;
import sun.net.util.IPAddressUtil;

import java.net.MalformedURLException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * URL .
 * HTTP URL 处理.
 *
 * @author sixh
 */
@Getter

public class URL {
    private String protocol;
    private String host;
    private Integer port = -1;
    private String path;
    private String query;
    private Map<String, String> parameters;
    private String full;
    private static final String AMPERSAND = "&";
    private static final char NUMBER = '#';
    private static final char COLON = ':';
    private static final char VIRGULE = '/';
    private static final char SQUARE_BRACKETS_LEFT = '[';
    private static final char SQUARE_BRACKETS_RIGHT = ']';
    private static final char QUESTION_MARK = '?';
    private static final char AT = '@';
    private static final char EQ = '=';
    private static final char EMPTY = ' ';
    private static final String URI_PROTOCOL = "url:";

    /**
     * 生成一个url .
     *
     * @param host       host.
     * @param port       port.
     * @param path       path.
     * @param parameters parameters.
     * @return URL.
     */
    public static URL valueOf(String protocol, String host, Integer port, String path, Map<String, String> parameters) {
        protocol = protocol.toLowerCase();
        URL url = new URL();
        url.protocol = protocol;
        if (host != null) {

            if (host.indexOf(':') >= 0 && !host.startsWith("[")) {
                host = "[" + host + "]";
            }
            url.host = host;
            url.port = port < 0 ? 0 : port;
        }
        url.path = path;
        url.parameters = parameters != null ? Collections.unmodifiableMap(parameters) : Collections.emptyMap();
        return url;
    }

    public String fullString() {
        StringBuilder sb = new StringBuilder();
        if (StringUtils.isNotBlank(protocol)) {

        }
        return "";
    }

    public static URL parse(String url) {
        URL newUrl = new URL();
        try {
            newUrl.url(url);
            newUrl.full = url;
            String query = newUrl.query;
            if (query != null && query.trim().length() > 0) {
                String[] split = query.split(AMPERSAND);
                Map<String, String> params = new HashMap<>(split.length);
                Arrays.stream(split)
                        .filter(e -> e.length() > 0)
                        .forEach(e -> {
                            int partIndex;
                            if ((partIndex = e.indexOf(EQ)) > 0) {
                                String left = e.substring(0, partIndex);
                                String right = e.substring(partIndex + 1);
                                params.put(left, right);
                            } else {
                                params.put(e, "");
                            }
                        });
                newUrl.parameters = params;
            }

        } catch (Exception e) {
            throw new IllegalArgumentException("invalid url");
        }
        return newUrl;
    }

    private void url(String spec) {
        int i, limit, c;
        int start = 0;
        String newProtocol = null;
        boolean aRef = false;
        try {
            limit = spec.length();
            while ((limit > 0) && (spec.charAt(limit - 1) <= EMPTY)) {
                limit--;        //eliminate trailing whitespace
            }
            while ((start < limit) && (spec.charAt(start) <= EMPTY)) {
                start++;        // eliminate leading whitespace
            }

            if (spec.regionMatches(true, start, URI_PROTOCOL, 0, 4)) {
                start += 4;
            }
            if (start < spec.length() && spec.charAt(start) == NUMBER) {
                /* we're assuming this is a ref relative to the context URL.
                 * This means protocols cannot start w/ '#', but we must parse
                 * ref URL's like: "hello:there" w/ a ':' in them.
                 */
                aRef = true;
            }
            for (i = start; !aRef && (i < limit) &&
                    ((c = spec.charAt(i)) != VIRGULE); i++) {
                if (c == COLON) {

                    String s = spec.substring(start, i).toLowerCase();
                    if (isValidProtocol(s)) {
                        newProtocol = s;
                        start = i + 1;
                    }
                    break;
                }
            }

            // Only use our context if the protocols match.
            protocol = newProtocol;
            if (protocol == null) {
                throw new MalformedURLException("no protocol: " + spec);
            }
            i = spec.indexOf(NUMBER, start);
            if (i >= 0) {
                limit = i;
            }
            parseUrl(this, spec, start, limit);
        } catch (Exception e) {
            throw new RuntimeException("url error", e);
        }
    }

    private void parseUrl(URL u, String spec, int start, int limit) {
        String protocol = u.getProtocol();
        String host = u.getHost();
        int port = u.getPort();
        String path = u.getPath();
        String query = "";
        boolean isRelPath = false;
        boolean queryOnly = false;
        if (start < limit) {
            int queryStart = spec.indexOf(QUESTION_MARK);
            queryOnly = queryStart == start;
            if ((queryStart != -1) && (queryStart < limit)) {
                query = spec.substring(queryStart + 1, limit);
                limit = queryStart;
                spec = spec.substring(0, queryStart);
            }
        }
        int i;
        boolean isUncName = (start <= limit - 4) &&
                (spec.charAt(start) == VIRGULE) &&
                (spec.charAt(start + 1) == VIRGULE) &&
                (spec.charAt(start + 2) == VIRGULE) &&
                (spec.charAt(start + 3) == VIRGULE);
        if (!isUncName && (start <= limit - 2) && (spec.charAt(start) == VIRGULE) &&
                (spec.charAt(start + 1) == VIRGULE)) {
            start += 2;
            i = spec.indexOf(VIRGULE, start);
            if (i < 0 || i > limit) {
                i = spec.indexOf(QUESTION_MARK, start);
                if (i < 0 || i > limit) {
                    i = limit;
                }
            }

            host = spec.substring(start, i);

            int ind = host.indexOf(AT);
            if (ind != -1) {
                if (ind != host.lastIndexOf(AT)) {
                    host = null;
                } else {
                    host = host.substring(ind + 1);
                }
            }
            if (host != null) {
                if (host.length() > 0 && (host.charAt(0) == SQUARE_BRACKETS_LEFT)) {
                    if ((ind = host.indexOf(SQUARE_BRACKETS_RIGHT)) > 2) {

                        String nhost = host;
                        host = nhost.substring(0, ind + 1);
                        if (!IPAddressUtil.
                                isIPv6LiteralAddress(host.substring(1, ind))) {
                            throw new IllegalArgumentException(
                                    "Invalid host: " + host);
                        }

                        port = -1;
                        if (nhost.length() > ind + 1) {
                            if (nhost.charAt(ind + 1) == COLON) {
                                ++ind;
                                // port can be null according to RFC2396
                                if (nhost.length() > (ind + 1)) {
                                    port = Integer.parseInt(nhost.substring(ind + 1));
                                }
                            } else {
                                throw new IllegalArgumentException(
                                        "Invalid authority field: " + host);
                            }
                        }
                    } else {
                        throw new IllegalArgumentException(
                                "Invalid authority field: " + host);
                    }
                } else {
                    ind = host.indexOf(COLON);
                    port = -1;
                    if (ind >= 0) {
                        // port can be null according to RFC2396
                        if (host.length() > (ind + 1)) {
                            port = Integer.parseInt(host.substring(ind + 1));
                        }
                        host = host.substring(0, ind);
                    }
                }
            } else {
                host = "";
            }
            if (port < -1) {
                throw new IllegalArgumentException("Invalid port number :" +
                        port);
            }
            start = i;
            if (host.length() > 0) {
                path = "";
            }
        }

        if (host == null) {
            host = "";
        }

        // Parse the file path if any
        if (start < limit) {
            if (spec.charAt(start) == VIRGULE) {
                path = spec.substring(start, limit);
            } else {
                String seperator = String.valueOf(VIRGULE);
                path = seperator + spec.substring(start, limit);
            }
        } else if (queryOnly && path != null) {
            int ind = path.lastIndexOf(VIRGULE);
            if (ind < 0) {
                ind = 0;
            }
            path = path.substring(0, ind) + VIRGULE;
        }
        if (path == null) {
            path = "";
        }
        this.protocol = protocol;
        this.path = path;
        this.port = port;
        this.host = host;
        this.query = query;
    }

    private boolean isValidProtocol(String protocol) {
        int len = protocol.length();
        if (len < 1) {
            return false;
        }
        char c = protocol.charAt(0);
        if (!Character.isLetter(c)) {
            return false;
        }
        for (int i = 1; i < len; i++) {
            c = protocol.charAt(i);
            if (!Character.isLetterOrDigit(c) && c != '.' && c != '+' &&
                    c != '-') {
                return false;
            }
        }
        return true;
    }

    @Override
    public String toString() {
        return full;
    }
}
