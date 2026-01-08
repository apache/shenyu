package org.apache.shenyu.plugin.base.route;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.SmartCacheManager;
import org.apache.shenyu.plugin.base.context.PluginExecutionContext;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpMethod;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.net.URI;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Test for DefaultRouteResolver.
 */
@ExtendWith(MockitoExtension.class)
class DefaultRouteResolverTest {

    @Mock
    private SmartCacheManager cacheManager;

    @Mock
    private ServerWebExchange exchange;

    @Mock
    private ServerHttpRequest request;

    private DefaultRouteResolver routeResolver;

    @BeforeEach
    void setUp() {
        routeResolver = new DefaultRouteResolver(cacheManager);

        // Setup mock exchange
        when(exchange.getRequest()).thenReturn(request);
        when(request.getURI()).thenReturn(URI.create("http://localhost:8080/api/users"));
        when(request.getMethod()).thenReturn(HttpMethod.GET);
    }

    @Test
    void testMatchSelectorFromCache() {
        // Given
        PluginExecutionContext context = PluginExecutionContext.fromExchange(exchange);
        SelectorData cachedSelector = createTestSelector("test-selector", true);

        when(cacheManager.getOrLoad(
                eq(SmartCacheManager.CacheTypes.SELECTOR),
                anyString(),
                any(Mono.class)
        )).thenReturn(Mono.just(cachedSelector));

        // When
        Mono<Optional<SelectorData>> result = routeResolver.matchSelector(
                context, Collections.singletonList(cachedSelector));

        // Then
        StepVerifier.create(result)
                .assertNext(opt -> {
                    assertTrue(opt.isPresent());
                    assertEquals("test-selector", opt.get().getName());
                })
                .verifyComplete();
    }

    @Test
    void testMatchSelectorNoMatch() {
        // Given
        PluginExecutionContext context = PluginExecutionContext.fromExchange(exchange);

        when(cacheManager.getOrLoad(
                eq(SmartCacheManager.CacheTypes.SELECTOR),
                anyString(),
                any(Mono.class)
        )).thenReturn(Mono.empty());

        // When
        Mono<Optional<SelectorData>> result = routeResolver.matchSelector(
                context, Collections.emptyList());

        // Then
        StepVerifier.create(result)
                .assertNext(opt -> assertFalse(opt.isPresent()))
                .verifyComplete();
    }

    @Test
    void testMatchRuleFromCache() {
        // Given
        PluginExecutionContext context = PluginExecutionContext.fromExchange(exchange);
        SelectorData selector = createTestSelector("test-selector", true);
        RuleData cachedRule = createTestRule("test-rule", true);

        when(cacheManager.getOrLoad(
                eq(SmartCacheManager.CacheTypes.RULE),
                anyString(),
                any(Mono.class)
        )).thenReturn(Mono.just(cachedRule));

        // When
        Mono<Optional<RuleData>> result = routeResolver.matchRule(
                context, selector, Collections.singletonList(cachedRule));

        // Then
        StepVerifier.create(result)
                .assertNext(opt -> {
                    assertTrue(opt.isPresent());
                    assertEquals("test-rule", opt.get().getName());
                })
                .verifyComplete();
    }

    @Test
    void testResolveRouteSuccess() {
        // Given
        PluginExecutionContext context = PluginExecutionContext.fromExchange(exchange);
        String pluginName = "test-plugin";

        SelectorData selector = createTestSelector("test-selector", true);
        selector.setContinued(true);
        RuleData rule = createTestRule("test-rule", true);

        when(cacheManager.getOrLoad(
                eq(SmartCacheManager.CacheTypes.SELECTOR),
                anyString(),
                any(Mono.class)
        )).thenReturn(Mono.just(selector));

        when(cacheManager.getOrLoad(
                eq(SmartCacheManager.CacheTypes.RULE),
                anyString(),
                any(Mono.class)
        )).thenReturn(Mono.just(rule));

        // When
        Mono<RouteResult> result = routeResolver.resolveRoute(context, pluginName);

        // Then
        StepVerifier.create(result)
                .assertNext(routeResult -> {
                    assertTrue(routeResult.isMatched());
                    assertTrue(routeResult.hasSelector());
                    assertTrue(routeResult.hasRule());
                })
                .verifyComplete();
    }

    @Test
    void testResolveRouteNoMatch() {
        // Given
        PluginExecutionContext context = PluginExecutionContext.fromExchange(exchange);
        String pluginName = "test-plugin";

        when(cacheManager.getOrLoad(
                eq(SmartCacheManager.CacheTypes.SELECTOR),
                anyString(),
                any(Mono.class)
        )).thenReturn(Mono.empty());

        // When
        Mono<RouteResult> result = routeResolver.resolveRoute(context, pluginName);

        // Then
        StepVerifier.create(result)
                .assertNext(routeResult -> {
                    assertFalse(routeResult.isMatched());
                    assertEquals(RouteResult.MatchType.NONE, routeResult.getMatchType());
                })
                .verifyComplete();
    }

    @Test
    void testIsPluginApplicable() {
        // Given
        PluginExecutionContext context = PluginExecutionContext.fromExchange(exchange);
        String pluginName = "test-plugin";

        // When & Then
        Mono<Boolean> result = routeResolver.isPluginApplicable(context, pluginName);

        StepVerifier.create(result)
                .expectNext(false) // BaseDataCache will return null by default
                .verifyComplete();
    }

    // Helper methods
    private SelectorData createTestSelector(String name, boolean enabled) {
        SelectorData selector = new SelectorData();
        selector.setId("selector-id-1");
        selector.setName(name);
        selector.setEnabled(enabled);
        selector.setPluginName("test-plugin");
        selector.setMatchMode(0);
        selector.setConditionList(new ArrayList<>());
        selector.setContinued(false);
        return selector;
    }

    private RuleData createTestRule(String name, boolean enabled) {
        RuleData rule = new RuleData();
        rule.setId("rule-id-1");
        rule.setName(name);
        rule.setEnabled(enabled);
        rule.setPluginName("test-plugin");
        rule.setMatchMode(0);
        rule.setConditionDataList(new ArrayList<>());
        return rule;
    }
}
