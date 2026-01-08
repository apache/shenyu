package org.apache.shenyu.plugin.base.context;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.server.reactive.ServerHttpRequest;
import org.springframework.web.server.ServerWebExchange;

import java.net.URI;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * Test for PluginExecutionContext.
 */
@ExtendWith(MockitoExtension.class)
class PluginExecutionContextTest {

    @Mock
    private ServerWebExchange exchange;

    @Mock
    private ServerHttpRequest request;

    @BeforeEach
    void setUp() {
        when(exchange.getRequest()).thenReturn(request);
    }

    @Test
    void testFromExchange() {
        // Given
        URI uri = URI.create("http://localhost:8080/api/users");
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Type", "application/json");
        headers.add("Authorization", "Bearer token123");

        when(request.getURI()).thenReturn(uri);
        when(request.getMethod()).thenReturn(HttpMethod.POST);
        when(request.getHeaders()).thenReturn(headers);

        // When
        PluginExecutionContext context = PluginExecutionContext.fromExchange(exchange);

        // Then
        assertNotNull(context);
        assertEquals("/api/users", context.getRequestPath());
        assertEquals("POST", context.getMethod());
        assertEquals(exchange, context.getExchange());
    }

    @Test
    void testGetHeader() {
        // Given
        URI uri = URI.create("http://localhost:8080/api/users");
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Type", "application/json");
        headers.add("X-Custom-Header", "custom-value");

        when(request.getURI()).thenReturn(uri);
        when(request.getMethod()).thenReturn(HttpMethod.GET);
        when(request.getHeaders()).thenReturn(headers);

        PluginExecutionContext context = PluginExecutionContext.fromExchange(exchange);

        // When & Then
        assertTrue(context.getHeader("content-type").isPresent());
        assertEquals("application/json", context.getHeader("content-type").get());

        assertTrue(context.getHeader("x-custom-header").isPresent());
        assertEquals("custom-value", context.getHeader("x-custom-header").get());

        assertFalse(context.getHeader("non-existent").isPresent());
    }

    @Test
    void testAttributes() {
        // Given
        URI uri = URI.create("http://localhost:8080/test");
        when(request.getURI()).thenReturn(uri);
        when(request.getMethod()).thenReturn(HttpMethod.GET);
        when(request.getHeaders()).thenReturn(new HttpHeaders());

        PluginExecutionContext context = PluginExecutionContext.fromExchange(exchange);

        // When
        context.setAttribute("key1", "value1");
        context.setAttribute("key2", 123);

        // Then
        assertTrue(context.getAttribute("key1").isPresent());
        assertEquals("value1", context.getAttribute("key1").get());

        assertTrue(context.getAttribute("key2").isPresent());
        assertEquals(123, context.getAttribute("key2").get());

        assertFalse(context.getAttribute("non-existent").isPresent());
    }

    @Test
    void testElapsedTime() throws InterruptedException {
        // Given
        URI uri = URI.create("http://localhost:8080/test");
        when(request.getURI()).thenReturn(uri);
        when(request.getMethod()).thenReturn(HttpMethod.GET);
        when(request.getHeaders()).thenReturn(new HttpHeaders());

        PluginExecutionContext context = PluginExecutionContext.fromExchange(exchange);

        // When
        Thread.sleep(10); // Wait a bit
        long elapsed = context.getElapsedTime();

        // Then
        assertTrue(elapsed >= 10, "Elapsed time should be at least 10ms");
        assertTrue(context.getStartTime() > 0);
    }

    @Test
    void testBuilder() {
        // Given
        URI uri = URI.create("http://localhost:8080/api/test");
        when(request.getURI()).thenReturn(uri);
        when(request.getMethod()).thenReturn(HttpMethod.PUT);
        when(request.getHeaders()).thenReturn(new HttpHeaders());

        // When
        PluginExecutionContext context = PluginExecutionContext.builder()
                .exchange(exchange)
                .requestPath("/api/test")
                .method("PUT")
                .attribute("custom", "value")
                .build();

        // Then
        assertNotNull(context);
        assertEquals("/api/test", context.getRequestPath());
        assertEquals("PUT", context.getMethod());
        assertTrue(context.getAttribute("custom").isPresent());
        assertEquals("value", context.getAttribute("custom").get());
    }

    @Test
    void testHeadersAreCaseInsensitive() {
        // Given
        URI uri = URI.create("http://localhost:8080/test");
        HttpHeaders headers = new HttpHeaders();
        headers.add("Content-Type", "application/json");

        when(request.getURI()).thenReturn(uri);
        when(request.getMethod()).thenReturn(HttpMethod.GET);
        when(request.getHeaders()).thenReturn(headers);

        PluginExecutionContext context = PluginExecutionContext.fromExchange(exchange);

        // When & Then - headers should be case-insensitive
        assertTrue(context.getHeader("content-type").isPresent());
        assertTrue(context.getHeader("Content-Type").isPresent());
        assertTrue(context.getHeader("CONTENT-TYPE").isPresent());
    }
}
