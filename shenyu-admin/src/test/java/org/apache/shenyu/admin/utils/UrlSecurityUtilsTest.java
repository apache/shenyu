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
import org.apache.shenyu.admin.config.properties.SwaggerImportProperties;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test for {@link UrlSecurityUtils}.
 */
public class UrlSecurityUtilsTest {

    private static final String PARSER_CONFUSION_PAYLOAD = "http://127.0.0.1:6666\\@1.1.1.1";

    @ParameterizedTest
    @ValueSource(ints = {8080, 8081, 9090, 9091})
    public void shouldAllowPublicApplicationPorts(final int port) {
        assertDoesNotThrow(() -> UrlSecurityUtils.validateUrlForSSRF("http://8.8.8.8:" + port + "/v3/api-docs"));
    }

    @Test
    public void validateUrlForSSRFShouldRejectParserConfusionPayload() {
        assertThrows(IllegalArgumentException.class,
                () -> UrlSecurityUtils.validateUrlForSSRF(PARSER_CONFUSION_PAYLOAD));
    }

    @ParameterizedTest
    @ValueSource(strings = {"127.0.0.1", "127.1", "2130706433", "0.0.0.0", "10.0.0.1", "172.16.0.1", "192.168.0.1",
            "169.254.169.254", "100.64.0.1", "224.0.0.1", "239.1.2.3", "255.255.255.255", "[::1]", "[::]",
            "[fe80::1]", "[fc00::1]", "[fd00::1]", "[ff02::1]", "[::ffff:127.0.0.1]"})
    public void shouldRejectInternalLiteralsOnApplicationPorts(final String host) {
        assertThrows(IllegalArgumentException.class, () -> UrlSecurityUtils.validateUrlForSSRF("http://" + host + ":8080/docs"));
    }

    @ParameterizedTest
    @ValueSource(strings = {"127.0.0.1", "10.0.0.1", "169.254.169.254", "100.64.0.1", "239.1.2.3", "fc00::1", "fd00::1", "ff02::1"})
    public void shouldRejectEveryInternalDnsAnswer(final String address) throws UnknownHostException {
        List<InetAddress> answers = Arrays.asList(InetAddress.getByName("8.8.8.8"), InetAddress.getByName(address));
        assertThrows(IllegalArgumentException.class, () -> UrlSecurityUtils.validatedDns("http://example.test:8080/docs",
                new SwaggerImportProperties(), hostname -> answers));
    }

    @Test
    public void shouldResolveOnlyOnceAndFreezeValidatedAnswers() throws UnknownHostException {
        AtomicInteger resolutions = new AtomicInteger();
        InetAddress publicAddress = InetAddress.getByName("8.8.8.8");
        List<InetAddress> answers = new ArrayList<>(Collections.singletonList(publicAddress));
        Dns pinned = UrlSecurityUtils.validatedDns("http://fc-public.example:8080/docs", new SwaggerImportProperties(), hostname -> {
            resolutions.incrementAndGet();
            return answers;
        });
        answers.set(0, InetAddress.getByName("127.0.0.1"));
        assertEquals(Collections.singletonList(publicAddress), pinned.lookup("fc-public.example"));
        assertEquals(Collections.singletonList(publicAddress), pinned.lookup("fc-public.example"));
        assertEquals(1, resolutions.get());
        assertThrows(UnsupportedOperationException.class, () -> pinned.lookup("fc-public.example").clear());
        assertThrows(UnknownHostException.class, () -> pinned.lookup("other.example"));
    }

    @Test
    public void shouldRejectEmptyNullAndUnresolvableDnsAnswers() {
        SwaggerImportProperties policy = new SwaggerImportProperties();
        assertThrows(IllegalArgumentException.class, () -> UrlSecurityUtils.validatedDns("http://example.test", policy, host -> null));
        assertThrows(IllegalArgumentException.class, () -> UrlSecurityUtils.validatedDns("http://example.test", policy, host -> Collections.emptyList()));
        assertThrows(IllegalArgumentException.class, () -> UrlSecurityUtils.validatedDns("http://example.test", policy, host -> Collections.singletonList(null)));
        assertThrows(IllegalArgumentException.class, () -> UrlSecurityUtils.validatedDns("http://example.test", policy, host -> {
            throw new UnknownHostException("unavailable");
        }));
    }

    @Test
    public void shouldApplyAllowAndDenyPolicyBeforeResolving() {
        SwaggerImportProperties policy = new SwaggerImportProperties();
        policy.setAllowedPorts(Set.of(8080, 3306));
        Dns forbiddenLookup = host -> {
            throw new AssertionError("Rejected port must not trigger DNS");
        };
        assertThrows(IllegalArgumentException.class, () -> UrlSecurityUtils.validatedDns("http://example.test:3306", policy, forbiddenLookup));
        assertThrows(IllegalArgumentException.class, () -> UrlSecurityUtils.validatedDns("http://example.test:8081", policy, forbiddenLookup));
        assertDoesNotThrow(() -> UrlSecurityUtils.validatedDns("http://8.8.8.8:8080", policy, Dns.SYSTEM));
        policy.setDeniedPorts(Collections.emptySet());
        assertDoesNotThrow(() -> UrlSecurityUtils.validatedDns("http://8.8.8.8:3306", policy, Dns.SYSTEM));
        assertThrows(IllegalArgumentException.class, () -> UrlSecurityUtils.validatedDns("http://127.0.0.1:3306", policy, Dns.SYSTEM));
    }

    @ParameterizedTest
    @ValueSource(ints = {22, 25, 53, 3306, 5432, 6379, 2181, 9200})
    public void shouldContinueDenyingInfrastructurePorts(final int port) {
        assertThrows(IllegalArgumentException.class, () -> UrlSecurityUtils.validateUrlForSSRF("http://8.8.8.8:" + port));
    }

    @ParameterizedTest
    @ValueSource(strings = {"http://8.8.8.8", "https://8.8.8.8", "https://[2606:4700:4700::1111]:9090"})
    public void shouldAllowPublicAddressesAndDefaultPorts(final String url) {
        assertDoesNotThrow(() -> UrlSecurityUtils.validateUrlForSSRF(url));
    }
}
