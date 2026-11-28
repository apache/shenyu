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

package org.apache.shenyu.plugin.divide;

import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.dto.convert.rule.sticky.DeterministicKeyConfig;
import org.apache.shenyu.common.dto.convert.rule.sticky.StickySessionConfig;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.loadbalancer.entity.LoadBalanceData;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.base.utils.UpstreamLabelUtils;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.apache.shenyu.plugin.divide.sticky.DeterministicKeyExtractor;
import org.apache.shenyu.plugin.divide.sticky.StickySessionAffinity;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpCookie;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Verifies the deterministic key mode of the sticky configuration: a configured
 * key maps binding-less requests onto the admitted candidates through the
 * consistent hash ring, never bypasses admission, and leaves the plain
 * load-balancing behavior untouched when not configured (#8037).
 */
class DivideDeterministicKeyTest {

    private static final String KEY_HEADER = "X-User-Id";

    private final DeterministicKeyExtractor extractor = new DeterministicKeyExtractor();

    private final StickySessionAffinity affinity = new StickySessionAffinity();

    private RuleData rule;

    private SelectorData selector;

    private DivideRuleHandle handle;

    private StickySessionConfig sticky;

    private DividePlugin plugin;

    @BeforeEach
    void setUp() {
        plugin = new DividePlugin();
        rule = new RuleData();
        rule.setId("deterministic-rule");
        rule.setSelectorId("deterministic-selector");
        selector = new SelectorData();
        selector.setId(rule.getSelectorId());
        rule.setHandle(GsonUtils.getGson().toJson(new DivideRuleHandle()));
        new DividePluginDataHandler().handlerRule(rule);
        handle = DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(rule));
        sticky = new StickySessionConfig();
        sticky.setEnabled(true);
        sticky.setSecrets(List.of("current-secret"));
        handle.setSticky(sticky);
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(org.apache.shenyu.plugin.api.result.ShenyuResult.class))
                .thenReturn(new org.apache.shenyu.plugin.api.result.DefaultShenyuResult());
        org.apache.shenyu.plugin.api.utils.SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    @AfterEach
    void tearDown() {
        new DividePluginDataHandler().removeRule(rule);
        UpstreamCacheManager.getInstance().removeByKey(selector.getId());
    }

    @Test
    void keyConfigurationParsesFromTheRuleHandleJson() {
        final DivideRuleHandle parsed = GsonUtils.getInstance().fromJson(
                "{\"sticky\":{\"enabled\":true,\"secrets\":[\"s\"],\"key\":{\"type\":\"COOKIE\",\"name\":\"sid\"}}}",
                DivideRuleHandle.class);
        assertNotNull(parsed.getSticky());
        assertNotNull(parsed.getSticky().getKey(), "the nested deterministic key must parse from the rule handle");
        assertEquals(DeterministicKeyConfig.KeyType.COOKIE, parsed.getSticky().getKey().getType());
        assertEquals("sid", parsed.getSticky().getKey().getName());
    }

    @Test
    void headerKeyMapsDeterministicallyToTheAdmittedCandidates() {
        final Upstream first = upstream("a:8080");
        final Upstream second = upstream("b:8080");
        configureKey(DeterministicKeyConfig.KeyType.HEADER, KEY_HEADER);
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(first, second))) {
            final String firstDomain = visit(headerRequest(Map.of(KEY_HEADER, "user-7")));
            final String secondDomain = visit(headerRequest(Map.of(KEY_HEADER, "user-7")));
            assertEquals(firstDomain, secondDomain, "the same key must map to the same target");
            assertTrue(List.of("http://a:8080", "http://b:8080").contains(firstDomain),
                    "the key must map onto the admitted candidates");
            // a different key is allowed to land on the other target
            final String otherDomain = visit(headerRequest(Map.of(KEY_HEADER, "user-8")));
            assertNotNull(otherDomain);
        }
    }

    @Test
    void missingKeyFallsBackToTheBaseAlgorithm() {
        final Upstream only = upstream("a:8080");
        configureKey(DeterministicKeyConfig.KeyType.HEADER, KEY_HEADER);
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(only))) {
            final String domain = visit(headerRequest(Map.of()));
            assertEquals("http://a:8080", domain, "a request without the key must still be routed");
        }
    }

    @Test
    void bindingTakesPrecedenceOverTheDeterministicKey() {
        final Upstream keyed = upstream("a:8080");
        final Upstream bound = upstream("b:8080");
        configureKey(DeterministicKeyConfig.KeyType.HEADER, KEY_HEADER);
        final String cookie = boundCookie(bound);
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(keyed, bound))) {
            final ServerWebExchange exchange = exchange(null, cookie, Map.of(KEY_HEADER, "user-7"));
            StepVerifier.create(plugin.doExecute(exchange, current -> Mono.empty(), selector, rule)).verifyComplete();
            assertEquals("http://b:8080", exchange.getAttribute(Constants.HTTP_DOMAIN),
                    "an explicit binding must win over the deterministic key");
        }
    }

    @Test
    void keyedPickDoesNotBypassBreakerAdmission() {
        final Upstream blocked = upstream("a:8080");
        final Upstream available = upstream("b:8080");
        configureKey(DeterministicKeyConfig.KeyType.HEADER, KEY_HEADER);
        final UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();
        final String key = UpstreamCircuitBreaker.buildKey(selector.getId(), blocked);
        for (int i = 0; i < UpstreamCircuitBreaker.DEFAULT_FAILURE_THRESHOLD; i++) {
            breaker.recordFailure(key);
        }
        try {
            try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(available, blocked))) {
                final ServerWebExchange exchange = headerRequest(Map.of(KEY_HEADER, "user-7"));
                StepVerifier.create(plugin.doExecute(exchange, current -> Mono.empty(), selector, rule)).verifyComplete();
                assertEquals("http://b:8080", exchange.getAttribute(Constants.HTTP_DOMAIN),
                        "a deterministic key must never bypass circuit-breaker admission");
            }
        } finally {
            breaker.reset(key);
        }
    }

    @Test
    void keyedPickReservesAndReleasesTheP2cAttempt() {
        final Upstream first = upstream("a:8080");
        final Upstream second = upstream("b:8080");
        configureKey(DeterministicKeyConfig.KeyType.HEADER, KEY_HEADER);
        handle.setLoadBalance(LoadBalanceEnum.P2C.getName());
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(first, second))) {
            final ServerWebExchange exchange = headerRequest(Map.of(KEY_HEADER, "user-7"));
            StepVerifier.create(plugin.doExecute(exchange, current -> {
                final String domain = current.getAttribute(Constants.HTTP_DOMAIN);
                final Upstream picked = "http://a:8080".equals(domain) ? first : second;
                assertEquals(2L, picked.getInflight().get(), "a keyed pick must reserve the P2C attempt");
                return Mono.empty();
            }, selector, rule)).verifyComplete();
            assertEquals(1L, first.getInflight().get(), "inflight must return to its baseline");
            assertEquals(1L, second.getInflight().get(), "inflight must return to its baseline");
        }
    }

    @Test
    void extractorReadsHeaderAndCookieLocationsAndTreatsBlankAsMissing() {
        final DeterministicKeyConfig headerKey = new DeterministicKeyConfig();
        headerKey.setType(DeterministicKeyConfig.KeyType.HEADER);
        headerKey.setName(KEY_HEADER);
        final ServerWebExchange headerExchange = headerRequest(Map.of(KEY_HEADER, "user-7"));
        assertEquals("user-7", extractor.extract(headerExchange, headerKey));

        final DeterministicKeyConfig cookieKey = new DeterministicKeyConfig();
        cookieKey.setType(DeterministicKeyConfig.KeyType.COOKIE);
        cookieKey.setName("session-key");
        final ServerWebExchange cookieExchange = MockServerWebExchange.from(
                MockServerHttpRequest.post("/test").cookie(new HttpCookie("session-key", "user-9"))
                        .header(KEY_HEADER, "decoy"));
        assertEquals("user-9", extractor.extract(cookieExchange, cookieKey),
                "a COOKIE key must read the cookie, not a same-named header");

        assertNull(extractor.extract(headerRequest(Map.of(KEY_HEADER, "   ")), headerKey),
                "a blank key behaves like a missing key");
        assertNull(extractor.extract(headerRequest(Map.of()), headerKey));
        final DeterministicKeyConfig unnamed = new DeterministicKeyConfig();
        assertNull(extractor.extract(headerExchange, unnamed), "no configured name, no extraction");
    }

    @Test
    void ringRemappingStaysWithinTheLiveCandidateSet() {
        final List<Upstream> full = List.of(upstream("a:8080"), upstream("b:8080"), upstream("c:8080"));
        final String key = "user-42";
        final Upstream initial = pickByKey(full, key);
        assertTrue(full.contains(initial));

        final List<Upstream> reduced = full.stream()
                .filter(upstream -> upstream != initial)
                .collect(Collectors.toList());
        final Upstream remapped = pickByKey(reduced, key);
        assertTrue(reduced.contains(remapped), "a key must land on the live candidates only");
    }

    @Test
    void ringDistributionCoversAllCandidatesReproducibly() {
        final List<Upstream> candidates = List.of(
                upstream("a:8080"), upstream("b:8080"), upstream("c:8080"), upstream("d:8080"));
        final Set<String> landed = IntStream.range(0, 64)
                .mapToObj(i -> pickByKey(candidates, "user-" + i).getUrl())
                .collect(Collectors.toSet());
        assertEquals(candidates.size(), landed.size(),
                "the ring must be able to reach every candidate; MD5 mapping is deterministic, so this is reproducible");
    }

    private Upstream pickByKey(final List<Upstream> candidates, final String key) {
        final LoadBalanceData data = new LoadBalanceData();
        data.setHashKey(key);
        return LoadBalancerFactory.selector(new ArrayList<>(candidates), org.apache.shenyu.common.enums.LoadBalanceEnum.HASH.getName(), data);
    }

    private void configureKey(final DeterministicKeyConfig.KeyType type, final String name) {
        final DeterministicKeyConfig key = new DeterministicKeyConfig();
        key.setType(type);
        key.setName(name);
        sticky.setKey(key);
    }

    private ServerWebExchange headerRequest(final Map<String, String> headers) {
        return exchange(null, null, headers);
    }

    private String visit(final ServerWebExchange exchange) {
        StepVerifier.create(plugin.doExecute(exchange, current -> Mono.empty(), selector, rule)).verifyComplete();
        final String domain = exchange.getAttribute(Constants.HTTP_DOMAIN);
        assertNotNull(domain, "every visit must be routed");
        return domain;
    }

    private ServerWebExchange exchange(final String domain, final String cookieValue, final Map<String, String> headers) {
        final MockServerHttpRequest.BodyBuilder request = MockServerHttpRequest.post("/test");
        if (Objects.nonNull(domain)) {
            request.header(Constants.SPECIFY_DOMAIN, domain);
        }
        if (Objects.nonNull(cookieValue)) {
            request.cookie(new HttpCookie(affinity.cookieName(rule.getId(), sticky), cookieValue));
        }
        headers.forEach(request::header);
        final ServerWebExchange exchange = MockServerWebExchange.from(request);
        exchange.getAttributes().put(Constants.CONTEXT, new org.apache.shenyu.plugin.api.context.ShenyuContext());
        return exchange;
    }

    private String boundCookie(final Upstream upstream) {
        final ServerWebExchange writing = exchange(null, null, Map.of());
        affinity.writeCookie(writing, rule.getId(), upstream, sticky);
        final String setCookie = writing.getResponse().getHeaders().getFirst(org.springframework.http.HttpHeaders.SET_COOKIE);
        final int nameEnd = setCookie.indexOf('=') + 1;
        final int attributes = setCookie.indexOf(';');
        return setCookie.substring(nameEnd, attributes);
    }

    private MockedStatic<UpstreamCacheManager> cache(final List<Upstream> upstreams) {
        final MockedStatic<UpstreamCacheManager> cache = org.mockito.Mockito.mockStatic(UpstreamCacheManager.class);
        final UpstreamCacheManager manager = mock(UpstreamCacheManager.class);
        // the cache contract hands filter() a mutable, request-owned snapshot
        when(manager.findUpstreamListByLabels(eq(selector.getId()), any()))
                .thenAnswer(invocation -> UpstreamLabelUtils.filter(upstreams, invocation.getArgument(1)));
        final List<Upstream> gray = upstreams.stream().filter(Upstream::isGray).toList();
        when(manager.findUpstreamListBySelectorId(selector.getId())).thenReturn(
                gray.isEmpty() ? new ArrayList<>(upstreams) : new ArrayList<>(gray));
        cache.when(UpstreamCacheManager::getInstance).thenReturn(manager);
        return cache;
    }

    private Upstream upstream(final String url) {
        final Upstream upstream = Upstream.builder().url(url).protocol("http://").weight(100).build();
        upstream.setLabels(Map.of());
        return upstream;
    }
}
