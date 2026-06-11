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

import java.io.Serializable;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.UnknownHostException;
import java.net.SocketException;
import java.net.NetworkInterface;
import java.util.Locale;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.List;
import java.util.Comparator;

/**
 * The type Ip utils.
 */
public final class IpUtils {
    
    private static final Logger LOG = LoggerFactory.getLogger(IpUtils.class);

    /**
     * ip pattern.
     */
    private static final Pattern IP_PATTERN = Pattern.compile("^((25[0-5]|2[0-4]\\d|[01]?\\d\\d?)($|(?!\\.$)\\.)){4}$");

    /**
     * net card pattern.
     */
    private static final Pattern NET_CARD_PATTERN = Pattern.compile("(\\d+)$");

    /**
     * Docker network interface name prefixes to filter out.
     */
    private static final List<String> DOCKER_NETWORK_PREFIXES = Arrays.asList("docker", "br-", "veth");

    /**
     * System env docker host ip.
     */
    private static final String SYSTEM_ENV_DOCKER_HOST_IP = "docker_host_ip";

    /**
     * Localhost.
     */
    private static final String LOCALHOST = "127.0.0.1";

    /**
     * priority of networkInterface when generating client ip.
     */
    private static final String PROPERTY = System.getProperty("networkInterface.priority", "enp<eth<bond");

    private static final List<String> PREFER_LIST = new ArrayList<>(Arrays.asList(PROPERTY.split("<")));

    private static final Comparator<NetCard> BY_NAME = (card1, card2) -> {
        int card1Score = -1;
        int card2Score = -1;
        for (String pre : PREFER_LIST) {
            if (card1.getName().contains(pre)) {
                card1Score = PREFER_LIST.indexOf(pre);
                break;
            }
        }
        for (String pre : PREFER_LIST) {
            if (card2.getName().contains(pre)) {
                card2Score = PREFER_LIST.indexOf(pre);
                break;
            }
        }
        return card2Score - card1Score;
    };

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
        String pattern = normalizeFilterPattern(filterHost);

        // if the progress works under docker environment
        // return the host ip about this docker located from environment value
        String dockerHostIp = getDockerHostIp();
        if (Objects.nonNull(dockerHostIp)) {
            return dockerHostIp;
        }

        return findHostFromNetworkInterfaces(pattern);
    }

    /**
     * Normalize filter pattern.
     *
     * @param filterHost host filterHost str
     * @return normalized pattern
     */
    private static String normalizeFilterPattern(final String filterHost) {
        if ("*".equals(filterHost) || (Objects.nonNull(filterHost) && filterHost.length() == 0)) {
            return null;
        }
        if (Objects.nonNull(filterHost) && !filterHost.contains("*") && !isCompleteHost(filterHost)) {
            return filterHost + "*";
        }
        return filterHost;
    }

    /**
     * Get Docker host IP from environment variables.
     *
     * @return Docker host IP if found, null otherwise
     */
    private static String getDockerHostIp() {
        String dockerHostIp = System.getenv(SYSTEM_ENV_DOCKER_HOST_IP);
        if (Objects.nonNull(dockerHostIp) && StringUtils.isNoneBlank(dockerHostIp)) {
            return dockerHostIp;
        }
        dockerHostIp = System.getenv(SYSTEM_ENV_DOCKER_HOST_IP.toUpperCase(Locale.ROOT));
        if (Objects.nonNull(dockerHostIp) && StringUtils.isNoneBlank(dockerHostIp)) {
            return dockerHostIp;
        }
        return null;
    }

    /**
     * Find host IP from network interfaces.
     *
     * @param pattern IP pattern to match
     * @return host IP address
     */
    private static String findHostFromNetworkInterfaces(final String pattern) {
        List<NetCard> ipv4Result = new ArrayList<>();
        List<NetCard> ipv6Result = new ArrayList<>();
        try {
            scanNetworkInterfaces(ipv4Result, ipv6Result);
            sortNetCards(ipv4Result, ipv6Result);
            return selectHostIp(ipv4Result, ipv6Result, pattern);
        } catch (SocketException ignore) {
            return LOCALHOST;
        }
    }

    /**
     * Scan network interfaces and collect IP addresses.
     *
     * @param ipv4Result IPv4 result list
     * @param ipv6Result IPv6 result list
     * @throws SocketException if network interface access fails
     */
    private static void scanNetworkInterfaces(final List<NetCard> ipv4Result, final List<NetCard> ipv6Result) 
            throws SocketException {
        Enumeration<NetworkInterface> enumeration = NetworkInterface.getNetworkInterfaces();
        while (enumeration.hasMoreElements()) {
            final NetworkInterface networkInterface = enumeration.nextElement();
            // Skip Docker network interfaces (e.g., docker0, br-xxx, vethxxx)
            if (isDockerNetworkInterface(networkInterface.getName())) {
                continue;
            }
            Enumeration<InetAddress> addresses = networkInterface.getInetAddresses();
            while (addresses.hasMoreElements()) {
                InetAddress inetAddress = addresses.nextElement();
                if (Objects.nonNull(inetAddress) && !inetAddress.isLoopbackAddress()) {
                    addNetCard(inetAddress, networkInterface.getName(), ipv4Result, ipv6Result);
                }
            }
        }
    }

    /**
     * Add network card to appropriate result list.
     *
     * @param inetAddress inet address
     * @param interfaceName network interface name
     * @param ipv4Result IPv4 result list
     * @param ipv6Result IPv6 result list
     */
    private static void addNetCard(final InetAddress inetAddress, final String interfaceName,
                                    final List<NetCard> ipv4Result, final List<NetCard> ipv6Result) {
        String hostAddress = inetAddress.getHostAddress();
        if (inetAddress instanceof Inet4Address && isCompleteHost(hostAddress)) {
            int ipv4Postfix = 0;
            try {
                String[] segments = hostAddress.split("\\.");
                ipv4Postfix = Integer.parseInt(segments[3]);
            } catch (NumberFormatException | ArrayIndexOutOfBoundsException e) {
                // Log the error if desired, or leave as default 0
            }
            NetCard netCard = new NetCard(hostAddress,
                    getName(interfaceName),
                    getNamePostfix(interfaceName),
                    ipv4Postfix);
            ipv4Result.add(netCard);
        } else {
            NetCard netCard = new NetCard(hostAddress,
                    getName(interfaceName),
                    getNamePostfix(interfaceName));
            ipv6Result.add(netCard);
        }
    }

    /**
     * Sort network cards by priority.
     *
     * @param ipv4Result IPv4 result list
     * @param ipv6Result IPv6 result list
     */
    private static void sortNetCards(final List<NetCard> ipv4Result, final List<NetCard> ipv6Result) {
        Comparator<NetCard> byNamePostfix = Comparator.comparing(NetCard::getNamePostfix);
        Comparator<NetCard> byIpv4Postfix = (card1, card2) -> card2.getIpv4Postfix() - card1.getIpv4Postfix();
        ipv4Result.sort(BY_NAME.thenComparing(byNamePostfix).thenComparing(byIpv4Postfix));
        ipv6Result.sort(BY_NAME.thenComparing(byNamePostfix));
    }

    /**
     * Select host IP from sorted network cards.
     *
     * @param ipv4Result IPv4 result list
     * @param ipv6Result IPv6 result list
     * @param pattern IP pattern to match
     * @return selected host IP
     */
    private static String selectHostIp(final List<NetCard> ipv4Result, final List<NetCard> ipv6Result,
                                        final String pattern) {
        String hostIp = null;
        // prefer ipv4
        if (!ipv4Result.isEmpty()) {
            if (Objects.nonNull(pattern)) {
                for (NetCard card : ipv4Result) {
                    if (ipMatch(card.getIp(), pattern)) {
                        hostIp = card.getIp();
                        break;
                    }
                }
            } else {
                hostIp = ipv4Result.get(0).getIp();
            }
        } else if (!ipv6Result.isEmpty()) {
            hostIp = ipv6Result.get(0).getIp();
        }
        // If failed to find, fall back to localhost
        if (Objects.isNull(hostIp)) {
            try {
                hostIp = InetAddress.getLocalHost().getHostAddress();
            } catch (UnknownHostException ignore) {
                hostIp = LOCALHOST;
            }
        }
        return hostIp;
    }

    /**
     * Judge whether host is complete.
     *
     * @param host host ip
     * @return boolean
     */
    public static boolean isCompleteHost(final String host) {
        if (Objects.isNull(host)) {
            return false;
        }
        return IP_PATTERN.matcher(host).matches();
    }

    /**
     * do ip match.
     *
     * @param ip      network ip
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

    /**
     * Check if the network interface is a Docker-related interface.
     *
     * @param name network interface name
     * @return true if it's a Docker network interface
     */
    private static boolean isDockerNetworkInterface(final String name) {
        if (Objects.isNull(name)) {
            return false;
        }
        String lowerName = name.toLowerCase(Locale.ROOT);
        for (String prefix : DOCKER_NETWORK_PREFIXES) {
            if (lowerName.startsWith(prefix)) {
                return true;
            }
        }
        return false;
    }

    /**
     * To obtain a prefix.
     *
     * @param name network interface name
     * @return the name
     */
    private static String getName(final String name) {
        Matcher matcher = NET_CARD_PATTERN.matcher(name);
        if (matcher.find()) {
            return name.replace(matcher.group(), "");
        }
        return name;
    }

    /**
     * Get the last number.
     *
     * @param name network interface name
     * @return the name postfix
     */
    private static Integer getNamePostfix(final String name) {
        LOG.debug("getNamePostfix name: {}", name);
        Matcher matcher = NET_CARD_PATTERN.matcher(name);
        if (matcher.find()) {
            String numberStr = matcher.group();
            LOG.debug("getNamePostfix matcher.group(): {}", numberStr);
            // Limit the number length to avoid parsing very large numbers (e.g., Docker network interface names)
            // Common network interface suffixes are usually 1-4 digits (e.g., eth0, enp3s0)
            if (numberStr.length() > 4) {
                LOG.debug("getNamePostfix: number too long, ignoring: {}", numberStr);
                return -1;
            }
            try {
                return Integer.parseInt(numberStr);
            } catch (NumberFormatException e) {
                LOG.warn("getNamePostfix: failed to parse number '{}' from interface name '{}'", numberStr, name, e);
                return -1;
            }
        }
        return -1;
    }

    /**
     * get Zookeeper Url.
     * @param zookeeperUrl zookeeperUrl.
     * @return ip form zookeeperUrl
     */
    public static String getZookeeperHost(final String zookeeperUrl) {
        if (Objects.isNull(zookeeperUrl) || zookeeperUrl.isEmpty()) {
            return null;
        }
        try {
            URI uri = new URI(zookeeperUrl);
            String scheme = uri.getScheme();
            if (Objects.nonNull(scheme) && ("zookeeper".equals(scheme) || "zk".equals(scheme))) {
                return uri.getHost();
            }
        } catch (URISyntaxException ignored) {
        }

        String[] parts = zookeeperUrl.split(":");
        if (parts.length >= 1) {
            return parts[0];
        }
        return null;
    }

    /**
     * replace Zookeeper Url.
     * @param zookeeperUrl String zookeeper address
     * @param replacement ip form zookeeper address
     * @return full zookeeper address
     */
    public static String replaceZookeeperHost(final String zookeeperUrl, final String replacement) {
        String extractedHost = getZookeeperHost(zookeeperUrl);
        if (Objects.nonNull(extractedHost)) {
            if (isCompleteHost(extractedHost)) {
                return zookeeperUrl;
            }
            return zookeeperUrl.replaceFirst(extractedHost, replacement);
        }
        return zookeeperUrl;
    }

    private static class NetCard implements Serializable {

        private String ip;

        private String name;

        private Integer namePostfix;

        private Integer ipv4Postfix;

        NetCard(final String ip, final String name, final Integer namePostfix) {
            this.ip = ip;
            this.name = name;
            this.namePostfix = namePostfix;
        }

        NetCard(final String ip, final String name, final Integer namePostfix, final Integer postfix) {
            this.ip = ip;
            this.name = name;
            this.namePostfix = namePostfix;
            this.ipv4Postfix = postfix;
        }

        public String getIp() {
            return ip;
        }

        public void setIp(final String ip) {
            this.ip = ip;
        }

        public String getName() {
            return name;
        }

        public void setName(final String name) {
            this.name = name;
        }

        public Integer getIpv4Postfix() {
            return ipv4Postfix;
        }

        public Integer getNamePostfix() {
            return namePostfix;
        }

        public void setNamePostfix(final Integer namePostfix) {
            this.namePostfix = namePostfix;
        }

    }
}
