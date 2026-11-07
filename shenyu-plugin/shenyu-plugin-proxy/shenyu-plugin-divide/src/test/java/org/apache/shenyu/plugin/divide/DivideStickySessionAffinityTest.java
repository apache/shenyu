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

import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.dto.convert.rule.sticky.StickySessionConfig;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.api.context.ShenyuContext;
import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;
import org.apache.shenyu.plugin.base.utils.UpstreamLabelUtils;
import org.apache.shenyu.plugin.divide.handler.DividePluginDataHandler;
import org.apache.shenyu.plugin.divide.sticky.StickySessionAffinity;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpCookie;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.mock.http.server.reactive.MockServerHttpRequest;
import org.springframework.mock.web.server.MockServerWebExchange;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Verifies sticky session affinity composed with the existing load balancing:
 * bindings never bypass canary/breaker admission, failure policies and the
 * default-off behavior (#8037).
 */
class DivideStickySessionAffinityTest {

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
        rule.setId("sticky-rule");
        rule.setSelectorId("sticky-selector");
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
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult());
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    @AfterEach
    void tearDown() {
        new DividePluginDataHandler().removeRule(rule);
        UpstreamCacheManager.getInstance().removeByKey(selector.getId());
    }

    @Test
    void newSessionAllocatesWithBaseAlgorithmAndWritesSignedCookie() {
        final Upstream first = upstream("a:8080");
        final Upstream second = upstream("b:8080");
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(first, second))) {
            final ServerWebExchange exchange = exchange(null, null);
            StepVerifier.create(plugin.doExecute(exchange, current -> Mono.empty(), selector, rule)).verifyComplete();

            final String setCookie = exchange.getResponse().getHeaders().getFirst(HttpHeaders.SET_COOKIE);
            assertNotNull(setCookie, "a new session must receive the binding cookie");
            assertTrue(setCookie.contains("HttpOnly"), "the binding cookie must be HttpOnly");
            assertTrue(setCookie.contains("Path=/"), "the binding cookie must carry a path");
            assertTrue(setCookie.contains("Max-Age=3600"), "the binding cookie must expire with the binding");
            final String boundDomain = exchange.getAttribute(Constants.HTTP_DOMAIN);
            assertNotNull(boundDomain);

            // the written cookie resolves back to the same target inside the candidate set
            final StickySessionAffinity.Resolution resolved = affinity.resolve(exchangeWithCookie(cookieValue(setCookie)),
                    rule.getId(), sticky, List.of(first, second));
            assertTrue(resolved.isBindingPresent());
            assertNotNull(resolved.getBound());
            assertEquals(boundDomain, resolved.getBound().buildDomain());
        }
    }

    @Test
    void validBindingSticksToTheBoundTarget() {
        final Upstream first = upstream("a:8080");
        final Upstream second = upstream("b:8080");
        final String cookie = boundCookie(second);
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(first, second))) {
            for (int i = 0; i < 2; i++) {
                final ServerWebExchange exchange = exchange(null, cookie);
                StepVerifier.create(plugin.doExecute(exchange, current -> Mono.empty(), selector, rule)).verifyComplete();
                assertEquals("http://b:8080", exchange.getAttribute(Constants.HTTP_DOMAIN),
                        "a valid binding must pin both requests to the bound target");
            }
        }
    }

    @Test
    void unavailableBoundTargetRebindsWithinCandidates() {
        final Upstream remaining = upstream("a:8080");
        final Upstream gone = upstream("b:8080");
        final String cookie = boundCookie(gone);
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(remaining))) {
            final ServerWebExchange exchange = exchange(null, cookie);
            StepVerifier.create(plugin.doExecute(exchange, current -> Mono.empty(), selector, rule)).verifyComplete();
            assertEquals("http://a:8080", exchange.getAttribute(Constants.HTTP_DOMAIN),
                    "REBIND must select a permitted replacement");
            final String setCookie = exchange.getResponse().getHeaders().getFirst(HttpHeaders.SET_COOKIE);
            assertNotNull(setCookie, "the rebind must rewrite the binding cookie");
            assertNotEquals(cookie, cookieValue(setCookie), "the rewritten binding must point at the new target");
        }
    }

    @Test
    void breakerBlockedBoundTargetIsNotHonored() {
        final Upstream blocked = upstream("b:8080");
        final Upstream available = upstream("a:8080");
        final String cookie = boundCookie(blocked);
        final UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();
        final String key = UpstreamCircuitBreaker.buildKey(selector.getId(), blocked);
        for (int i = 0; i < UpstreamCircuitBreaker.DEFAULT_FAILURE_THRESHOLD; i++) {
            breaker.recordFailure(key);
        }
        try {
            try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(available, blocked))) {
                final ServerWebExchange exchange = exchange(null, cookie);
                StepVerifier.create(plugin.doExecute(exchange, current -> Mono.empty(), selector, rule)).verifyComplete();
                assertEquals("http://a:8080", exchange.getAttribute(Constants.HTTP_DOMAIN),
                        "a binding must never bypass circuit-breaker admission");
            }
        } finally {
            breaker.reset(key);
        }
    }

    @Test
    void strictPolicyRejectsWhenBoundTargetUnavailable() {
        sticky.setOnUnavailable("STRICT");
        final Upstream remaining = upstream("a:8080");
        final String cookie = boundCookie(upstream("b:8080"));
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(remaining))) {
            final ServerWebExchange exchange = exchange(null, cookie);
            StepVerifier.create(plugin.doExecute(exchange, current -> Mono.empty(), selector, rule)).verifyComplete();
            assertEquals(HttpStatus.SERVICE_UNAVAILABLE, exchange.getResponse().getStatusCode(),
                    "STRICT must reject instead of silently changing the session's target");
            assertNull(exchange.getAttribute(Constants.HTTP_DOMAIN));
        }
    }

    @Test
    void specifyDomainOverridesAffinity() {
        final Upstream first = upstream("a:8080");
        final Upstream second = upstream("b:8080");
        final String cookie = boundCookie(second);
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(first, second))) {
            final ServerWebExchange exchange = exchange("a:8080", cookie);
            StepVerifier.create(plugin.doExecute(exchange, current -> Mono.empty(), selector, rule)).verifyComplete();
            assertEquals("http://a:8080", exchange.getAttribute(Constants.HTTP_DOMAIN),
                    "an explicit specify-domain request is an operations override");
        }
    }

    @Test
    void defaultOffKeepsPlainBehavior() {
        handle.setSticky(null);
        final Upstream first = upstream("a:8080");
        final Upstream second = upstream("b:8080");
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(first, second))) {
            final ServerWebExchange exchange = exchange(null, null);
            StepVerifier.create(plugin.doExecute(exchange, current -> Mono.empty(), selector, rule)).verifyComplete();
            assertNotNull(exchange.getAttribute(Constants.HTTP_DOMAIN));
            assertNull(exchange.getResponse().getHeaders().getFirst(HttpHeaders.SET_COOKIE),
                    "disabled rules must not emit a binding cookie");
        }
    }

    @Test
    void leastActiveAccountingCompletesOnBindingHit() {
        handle.setLoadBalance("leastActive");
        final Upstream bound = upstream("b:8080");
        final Upstream other = upstream("a:8080");
        final String cookie = boundCookie(bound);
        final long baseline = bound.getInflight().get();
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(other, bound))) {
            final ServerWebExchange exchange = exchange(null, cookie);
            StepVerifier.create(plugin.doExecute(exchange, current -> {
                assertTrue(bound.getInflight().get() >= baseline + 1,
                        "a binding hit acquires the attempt accounting like any selection");
                return Mono.empty();
            }, selector, rule)).verifyComplete();
            assertEquals(baseline, bound.getInflight().get(),
                    "the accounting must return to baseline on completion");
        }
    }

    @Test
    void resolveRejectsTamperedExpiredAndForeignRuleCookies() {
        final Upstream target = upstream("b:8080");
        final String valid = boundCookie(target);

        final String tampered = valid.substring(0, valid.length() - 4) + "AAAA";
        final StickySessionAffinity.Resolution tamperedResult = affinity.resolve(exchangeWithCookie(tampered),
                rule.getId(), sticky, List.of(target));
        assertFalse(tamperedResult.isBindingPresent(), "a broken signature must be treated as a miss");

        final String expiredPayload = rule.getId() + "|" + target.identity().hashKey() + "|"
                + (System.currentTimeMillis() / 1000L - 10);
        final String expired = ReflectionTestUtils.invokeMethod(affinity, "encode", expiredPayload, "current-secret");
        final StickySessionAffinity.Resolution expiredResult = affinity.resolve(exchangeWithCookie(expired),
                rule.getId(), sticky, List.of(target));
        assertFalse(expiredResult.isBindingPresent(), "an expired binding must be treated as a miss");

        final StickySessionAffinity.Resolution foreignResult = affinity.resolve(exchangeWithCookie(valid),
                "another-rule", sticky, List.of(target));
        assertFalse(foreignResult.isBindingPresent(), "a cookie of another rule must not bind this one");
    }

    @Test
    void rotatedSecretStillHonorsBinding() {
        sticky.setSecrets(Arrays.asList("new-secret", "current-secret"));
        final Upstream target = upstream("b:8080");
        // a cookie signed with the previous secret
        final String cookie = ReflectionTestUtils.invokeMethod(affinity, "encode",
                rule.getId() + "|" + target.identity().hashKey() + "|"
                        + (System.currentTimeMillis() / 1000L + 3600), "current-secret");
        final StickySessionAffinity.Resolution resolved = affinity.resolve(exchangeWithCookie(cookie),
                rule.getId(), sticky, List.of(target));
        assertTrue(resolved.isBindingPresent(), "a cookie signed with a previous key must still verify");
        assertEquals(target.identity().hashKey(), resolved.getBound().identity().hashKey());
    }

    /**
     * STRICT rejects only a presented binding whose target is not eligible; a
     * request without a cookie is a new session and must be allocated (#8037).
     */
    @Test
    public void strictPolicyStillAllocatesNewSessions() {
        sticky.setOnUnavailable("STRICT");
        final Upstream only = upstream("a:8080");
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(only))) {
            final ServerWebExchange exchange = exchange(null, null);
            StepVerifier.create(plugin.doExecute(exchange, current -> Mono.empty(), selector, rule)).verifyComplete();
            assertEquals("http://a:8080", exchange.getAttribute(Constants.HTTP_DOMAIN),
                    "a new session must be allocated even under STRICT");
            assertNotEquals(HttpStatus.SERVICE_UNAVAILABLE, exchange.getResponse().getStatusCode());
        }
    }

    /**
     * A P2C binding hit skips the P2C selection, which normally reserves the
     * attempt: the same accounting must apply or the count drifts down.
     */
    @Test
    public void p2cBindingHitKeepsAccounting() {
        handle.setLoadBalance("p2c");
        final Upstream bound = upstream("b:8080");
        final Upstream other = upstream("a:8080");
        final String cookie = boundCookie(bound);
        final long baseline = bound.getInflight().get();
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(other, bound))) {
            final ServerWebExchange exchange = exchange(null, cookie);
            StepVerifier.create(plugin.doExecute(exchange, current -> {
                assertTrue(bound.getInflight().get() >= baseline + 1,
                        "a P2C binding hit must reserve the attempt like the P2C selection does");
                return Mono.empty();
            }, selector, rule)).verifyComplete();
            assertEquals(baseline, bound.getInflight().get(),
                    "the P2C accounting must return to baseline on completion");
            assertTrue(bound.getLastPicked() > 0, "the EWMA timestamp must be refreshed on a binding hit");
        }
    }

    /**
     * Broken configurations disable affinity instead of producing unusable cookies.
     */
    @Test
    public void invalidStickyConfigBehavesAsDisabled() {
        final Upstream only = upstream("a:8080");
        final String[][] broken = {null, new String[0], new String[]{"   "}};
        for (final String[] secrets : broken) {
            if (Objects.nonNull(secrets)) {
                sticky.setSecrets(Arrays.asList(secrets));
            } else {
                sticky.setSecrets(null);
            }
            try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(only))) {
                final ServerWebExchange exchange = exchange(null, null);
                StepVerifier.create(plugin.doExecute(exchange, current -> Mono.empty(), selector, rule)).verifyComplete();
                assertEquals("http://a:8080", exchange.getAttribute(Constants.HTTP_DOMAIN));
                assertNull(exchange.getResponse().getHeaders().getFirst(HttpHeaders.SET_COOKIE),
                        "secrets " + Arrays.toString(secrets) + " must disable affinity");
            }
        }
    }

    /**
     * The binding key is the upstream identity, so two upstreams sharing a URL
     * but differing in group must not absorb each other's binding (#8037).
     */
    @Test
    public void bindingRespectsUpstreamIdentity() {
        final Upstream plain = Upstream.builder().url("b:8080").protocol("http://").weight(100).build();
        plain.setLabels(Map.of());
        final Upstream grouped = Upstream.builder().url("b:8080").protocol("http://").weight(100).build();
        grouped.setGroup("grpc-group");
        grouped.setLabels(Map.of());
        final String cookie = boundCookie(grouped);
        final StickySessionAffinity.Resolution resolved = affinity.resolve(exchangeWithCookie(cookie),
                rule.getId(), sticky, List.of(plain, grouped));
        assertTrue(resolved.isBindingPresent());
        assertEquals("grpc-group", resolved.getBound().getGroup(),
                "the binding must resolve to the grouped instance, not the URL twin");
    }

    /**
     * The binding cookie is marked Secure when the request arrived over HTTPS.
     */
    @Test
    public void httpsRequestsMarkTheBindingCookieSecure() {
        final Upstream only = upstream("a:8080");
        try (MockedStatic<UpstreamCacheManager> cache = cache(List.of(only))) {
            final ServerWebExchange exchange = MockServerWebExchange.from(
                    MockServerHttpRequest.post("https://localhost/test"));
            exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
            StepVerifier.create(plugin.doExecute(exchange, current -> Mono.empty(), selector, rule)).verifyComplete();
            final String setCookie = exchange.getResponse().getHeaders().getFirst(HttpHeaders.SET_COOKIE);
            assertNotNull(setCookie);
            assertTrue(setCookie.contains("Secure"), "an HTTPS request must get a Secure binding cookie");
        }
    }

    /**
     * The default storage of a binding is scoped to its rule, so rules served
     * under the same host write different browser cookies (#8136).
     */
    @Test
    public void defaultCookieNameIsScopedToTheRule() {
        final StickySessionConfig first = new StickySessionConfig();
        final StickySessionConfig second = new StickySessionConfig();
        final String firstName = affinity.cookieName("rule-a", first);
        final String secondName = affinity.cookieName("rule-b", second);
        assertNotEquals(firstName, secondName, "rules must not share the default binding storage");
        assertTrue(firstName.startsWith(StickySessionConfig.DEFAULT_COOKIE_NAME + "-"),
                "the derived name must stay recognizable");
        assertEquals(firstName, affinity.cookieName("rule-a", new StickySessionConfig()),
                "the derived name must be stable for the same rule");
        assertFalse(affinity.cookieName("a b|rule", new StickySessionConfig()).matches(".*[\\s|].*"),
                "an arbitrary rule id must not leak into the cookie name");
        // rules stored before the fix persist the default name explicitly; they must be scoped too
        first.setCookieName(StickySessionConfig.DEFAULT_COOKIE_NAME);
        assertEquals(firstName, affinity.cookieName("rule-a", first),
                "the default base name must still derive a rule-scoped name");

        // any other configured name is used verbatim: rules may still opt into one shared cookie
        first.setCookieName("shared");
        second.setCookieName("shared");
        assertEquals("shared", affinity.cookieName("rule-a", first));
        assertEquals("shared", affinity.cookieName("rule-b", second));
    }

    /**
     * Interleaved traffic of two rules on one host retains each rule's own
     * binding: the second rule must not overwrite the first one's cookie, under
     * either failure policy (#8136).
     */
    @Test
    public void interleavedRulesKeepTheirOwnBindings() {
        final SelectorData selectorB = new SelectorData();
        selectorB.setId("sticky-selector-b");
        final RuleData ruleB = new RuleData();
        ruleB.setId("sticky-rule-b");
        ruleB.setSelectorId(selectorB.getId());
        ruleB.setHandle(GsonUtils.getGson().toJson(new DivideRuleHandle()));
        new DividePluginDataHandler().handlerRule(ruleB);
        final DivideRuleHandle handleB = DividePluginDataHandler.CACHED_HANDLE.get().obtainHandle(CacheKeyUtils.INST.getKey(ruleB));
        final StickySessionConfig stickyB = new StickySessionConfig();
        stickyB.setEnabled(true);
        stickyB.setSecrets(List.of("current-secret"));
        handleB.setSticky(stickyB);
        final List<Upstream> poolA = List.of(upstream("a1:8080"), upstream("a2:8080"));
        final List<Upstream> poolB = List.of(upstream("b1:8080"), upstream("b2:8080"));
        try {
            try (MockedStatic<UpstreamCacheManager> cache = cache(selector.getId(), poolA, selectorB.getId(), poolB)) {
                for (final String policy : List.of("REBIND", "STRICT")) {
                    sticky.setOnUnavailable(policy);
                    stickyB.setOnUnavailable(policy);
                    final Map<String, String> jar = new HashMap<>();
                    final String targetA = visit(rule, selector, jar);
                    final String targetB = visit(ruleB, selectorB, jar);
                    assertEquals(2, jar.size(), policy + ": each rule must own one binding cookie");
                    for (int i = 0; i < 2; i++) {
                        assertEquals(targetA, visit(rule, selector, jar),
                                policy + ": rule A must keep its binding after rule B is visited");
                        assertEquals(targetB, visit(ruleB, selectorB, jar),
                                policy + ": rule B must keep its binding after rule A is visited");
                    }
                    jar.clear();
                }
            }
        } finally {
            new DividePluginDataHandler().removeRule(ruleB);
        }
    }

    /**
     * Sends one request through the plugin with the browser cookie jar and
     * folds the response cookies back into it, then returns the chosen domain.
     */
    private String visit(final RuleData visited, final SelectorData visitedSelector, final Map<String, String> jar) {
        final MockServerHttpRequest.BodyBuilder request = MockServerHttpRequest.post("/test");
        jar.forEach((name, value) -> request.cookie(new HttpCookie(name, value)));
        final ServerWebExchange exchange = MockServerWebExchange.from(request);
        exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
        StepVerifier.create(plugin.doExecute(exchange, current -> Mono.empty(), visitedSelector, visited)).verifyComplete();
        final List<String> setCookies = exchange.getResponse().getHeaders().get(HttpHeaders.SET_COOKIE);
        if (Objects.nonNull(setCookies)) {
            for (final String setCookie : setCookies) {
                jar.put(setCookie.substring(0, setCookie.indexOf('=')), cookieValue(setCookie));
            }
        }
        final String domain = exchange.getAttribute(Constants.HTTP_DOMAIN);
        assertNotNull(domain, "every visit must be routed");
        return domain;
    }

    /** The cookie name the default configuration derives for this rule. */
    private String cookieName() {
        return affinity.cookieName(rule.getId(), sticky);
    }

    private String boundCookie(final Upstream upstream) {
        final ServerWebExchange writing = exchange(null, null);
        affinity.writeCookie(writing, rule.getId(), upstream, sticky);
        return cookieValue(writing.getResponse().getHeaders().getFirst(HttpHeaders.SET_COOKIE));
    }

    private String cookieValue(final String setCookie) {
        final int nameEnd = setCookie.indexOf('=') + 1;
        final int attributes = setCookie.indexOf(';');
        return setCookie.substring(nameEnd, attributes);
    }

    private ServerWebExchange exchangeWithCookie(final String cookieValue) {
        return exchange(null, cookieValue);
    }

    private MockedStatic<UpstreamCacheManager> cache(final List<Upstream> upstreams) {
        return cache(selector.getId(), upstreams);
    }

    private MockedStatic<UpstreamCacheManager> cache(final String selectorId, final List<Upstream> upstreams) {
        final MockedStatic<UpstreamCacheManager> cache = org.mockito.Mockito.mockStatic(UpstreamCacheManager.class);
        final UpstreamCacheManager manager = mock(UpstreamCacheManager.class);
        // the cache contract hands filter() a mutable, request-owned snapshot
        when(manager.findUpstreamListByLabels(eq(selectorId), any()))
                .thenAnswer(invocation -> UpstreamLabelUtils.filter(upstreams, invocation.getArgument(1)));
        final List<Upstream> gray = upstreams.stream().filter(Upstream::isGray).toList();
        when(manager.findUpstreamListBySelectorId(selectorId)).thenReturn(
                gray.isEmpty() ? new ArrayList<>(upstreams) : new ArrayList<>(gray));
        cache.when(UpstreamCacheManager::getInstance).thenReturn(manager);
        return cache;
    }

    private MockedStatic<UpstreamCacheManager> cache(final String firstSelectorId, final List<Upstream> first,
                                                     final String secondSelectorId, final List<Upstream> second) {
        final MockedStatic<UpstreamCacheManager> cache = org.mockito.Mockito.mockStatic(UpstreamCacheManager.class);
        final UpstreamCacheManager manager = mock(UpstreamCacheManager.class);
        when(manager.findUpstreamListByLabels(eq(firstSelectorId), any()))
                .thenAnswer(invocation -> UpstreamLabelUtils.filter(first, invocation.getArgument(1)));
        when(manager.findUpstreamListBySelectorId(firstSelectorId)).thenReturn(new ArrayList<>(first));
        when(manager.findUpstreamListByLabels(eq(secondSelectorId), any()))
                .thenAnswer(invocation -> UpstreamLabelUtils.filter(second, invocation.getArgument(1)));
        when(manager.findUpstreamListBySelectorId(secondSelectorId)).thenReturn(new ArrayList<>(second));
        cache.when(UpstreamCacheManager::getInstance).thenReturn(manager);
        return cache;
    }

    private ServerWebExchange exchange(final String domain, final String cookieValue) {
        // post() returns the concrete builder type that carries .cookie(); the plugin
        // only inspects method/URI/cookies here, never forwards in these tests
        final MockServerHttpRequest.BodyBuilder request = MockServerHttpRequest.post("/test");
        if (Objects.nonNull(domain)) {
            request.header(Constants.SPECIFY_DOMAIN, domain);
        }
        if (Objects.nonNull(cookieValue)) {
            request.cookie(new org.springframework.http.HttpCookie(cookieName(), cookieValue));
        }
        final ServerWebExchange exchange = MockServerWebExchange.from(request);
        exchange.getAttributes().put(Constants.CONTEXT, new ShenyuContext());
        return exchange;
    }

    private Upstream upstream(final String url) {
        final Upstream upstream = Upstream.builder().url(url).protocol("http://").weight(100).build();
        upstream.setLabels(Map.of());
        return upstream;
    }
}
