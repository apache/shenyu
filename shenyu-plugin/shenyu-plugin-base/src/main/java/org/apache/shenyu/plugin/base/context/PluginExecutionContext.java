package org.apache.shenyu.plugin.base.context;

import org.springframework.web.server.ServerWebExchange;

import java.net.URI;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Plugin execution context containing all information needed for plugin execution.
 *
 * This class encapsulates request information and execution state to avoid
 * repeated parsing and provide a clean interface for plugins.
 */
public final class PluginExecutionContext {

    private final ServerWebExchange exchange;
    private final String requestPath;
    private final String method;
    private final Map<String, String> headers;
    private final Map<String, Object> attributes;
    private final long startTime;

    private PluginExecutionContext(Builder builder) {
        this.exchange = builder.exchange;
        this.requestPath = builder.requestPath;
        this.method = builder.method;
        this.headers = Map.copyOf(builder.headers);
        this.attributes = new ConcurrentHashMap<>(builder.attributes);
        this.startTime = System.currentTimeMillis();
    }

    /**
     * Create context from ServerWebExchange.
     */
    public static PluginExecutionContext fromExchange(ServerWebExchange exchange) {
        return builder()
                .exchange(exchange)
                .requestPath(extractPath(exchange))
                .method(exchange.getRequest().getMethod().name())
                .headers(extractHeaders(exchange))
                .build();
    }

    /**
     * Get the original ServerWebExchange.
     */
    public ServerWebExchange getExchange() {
        return exchange;
    }

    /**
     * Get the request path (URI raw path).
     */
    public String getRequestPath() {
        return requestPath;
    }

    /**
     * Get HTTP method.
     */
    public String getMethod() {
        return method;
    }

    /**
     * Get request headers.
     */
    public Map<String, String> getHeaders() {
        return headers;
    }

    /**
     * Get a specific header value.
     */
    public Optional<String> getHeader(String name) {
        return Optional.ofNullable(headers.get(name.toLowerCase()));
    }

    /**
     * Get context attributes.
     */
    public Map<String, Object> getAttributes() {
        return attributes;
    }

    /**
     * Set context attribute.
     */
    public void setAttribute(String key, Object value) {
        attributes.put(key, value);
    }

    /**
     * Get context attribute.
     */
    @SuppressWarnings("unchecked")
    public <T> Optional<T> getAttribute(String key) {
        return Optional.ofNullable((T) attributes.get(key));
    }

    /**
     * Get execution start time.
     */
    public long getStartTime() {
        return startTime;
    }

    /**
     * Calculate elapsed time in milliseconds.
     */
    public long getElapsedTime() {
        return System.currentTimeMillis() - startTime;
    }

    // Helper methods
    private static String extractPath(ServerWebExchange exchange) {
        URI uri = exchange.getRequest().getURI();
        return uri.getRawPath();
    }

    private static Map<String, String> extractHeaders(ServerWebExchange exchange) {
        Map<String, String> headerMap = new ConcurrentHashMap<>();
        exchange.getRequest().getHeaders().forEach((name, values) -> {
            if (!values.isEmpty()) {
                headerMap.put(name.toLowerCase(), values.get(0));
            }
        });
        return headerMap;
    }

    // Builder pattern
    public static Builder builder() {
        return new Builder();
    }

    public static final class Builder {
        private ServerWebExchange exchange;
        private String requestPath;
        private String method;
        private Map<String, String> headers = new ConcurrentHashMap<>();
        private Map<String, Object> attributes = new ConcurrentHashMap<>();

        public Builder exchange(ServerWebExchange exchange) {
            this.exchange = exchange;
            return this;
        }

        public Builder requestPath(String requestPath) {
            this.requestPath = requestPath;
            return this;
        }

        public Builder method(String method) {
            this.method = method;
            return this;
        }

        public Builder headers(Map<String, String> headers) {
            this.headers.putAll(headers);
            return this;
        }

        public Builder attribute(String key, Object value) {
            this.attributes.put(key, value);
            return this;
        }

        public PluginExecutionContext build() {
            return new PluginExecutionContext(this);
        }
    }
}