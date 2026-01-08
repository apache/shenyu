package org.apache.shenyu.plugin.base;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.context.PluginExecutionContext;
import org.apache.shenyu.plugin.base.metrics.MetricsHelper;
import org.apache.shenyu.plugin.base.route.RouteResolver;
import org.apache.shenyu.plugin.base.route.RouteResult;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.Objects;

/**
 * Enhanced AbstractShenyuPlugin with separated responsibilities.
 *
 * Key improvements:
 * 1. Simplified execute() method using template method pattern
 * 2. Delegated routing logic to RouteResolver
 * 3. Built-in metrics collection
 * 4. Cleaner error handling
 * 5. Lazy initialization of dependencies
 *
 * Backward compatibility: This class maintains the same public interface
 * as the original AbstractShenyuPlugin, ensuring existing plugins work without changes.
 */
public abstract class EnhancedAbstractShenyuPlugin implements ShenyuPlugin {

    private static final Logger LOG = LoggerFactory.getLogger(EnhancedAbstractShenyuPlugin.class);

    // Lazy-initialized dependencies
    private volatile RouteResolver routeResolver;
    private volatile MetricsHelper metricsHelper;
    private volatile boolean initialized = false;

    /**
     * Template method for plugin execution.
     *
     * This method implements the main execution flow:
     * 1. Check plugin enabled status
     * 2. Create execution context
     * 3. Resolve routing (selector + rule)
     * 4. Execute plugin logic
     * 5. Record metrics and handle errors
     */
    @Override
    public final Mono<Void> execute(final ServerWebExchange exchange, final ShenyuPluginChain chain) {
        // Lazy initialization on first use
        if (!initialized) {
            initializeDependencies();
        }

        final String pluginName = named();

        return Mono.just(pluginName)
                .flatMap(this::checkPluginEnabled)
                .filter(enabled -> enabled)
                .flatMap(enabled -> createExecutionContext(exchange))
                .flatMap(context -> executeWithContext(context, chain))
                .switchIfEmpty(chain.execute(exchange)) // Plugin disabled or not applicable
                .doOnSuccess(v -> metricsHelper.recordSuccess(pluginName))
                .doOnError(error -> {
                    metricsHelper.recordError(pluginName, error);
                    LOG.error("Plugin [{}] execution failed", pluginName, error);
                })
                .onErrorResume(error -> handlePluginError(exchange, chain, error));
    }

    /**
     * Core plugin execution logic with context.
     */
    private Mono<Void> executeWithContext(PluginExecutionContext context, ShenyuPluginChain chain) {
        final String pluginName = named();
        final long startTime = System.nanoTime();

        return routeResolver.resolveRoute(context, pluginName)
                .flatMap(routeResult -> {
                    if (!routeResult.isMatched()) {
                        return handleNoMatch(context, chain);
                    }

                    // Record metrics for successful route resolution
                    metricsHelper.recordRouteMatch(pluginName, routeResult.getMatchType());

                    // Execute plugin business logic
                    return executePluginLogic(context, chain, routeResult);
                })
                .doFinally(signal -> {
                    long duration = System.nanoTime() - startTime;
                    metricsHelper.recordExecutionTime(pluginName, duration);
                })
                .contextWrite(ctx -> ctx.put("pluginName", pluginName));
    }

    /**
     * Execute the actual plugin business logic.
     */
    private Mono<Void> executePluginLogic(
            PluginExecutionContext context,
            ShenyuPluginChain chain,
            RouteResult routeResult) {

        SelectorData selector = routeResult.getSelector().orElse(null);
        RuleData rule = routeResult.getRule().orElse(createDefaultRule(selector));

        // Call the abstract method that subclasses implement
        return Mono.fromRunnable(() -> printExecutionLog(selector, rule, named()))
                .then(doExecute(context.getExchange(), chain, selector, rule));
    }

    /**
     * Abstract method that subclasses must implement.
     * This maintains backward compatibility with the original interface.
     */
    protected abstract Mono<Void> doExecute(
            ServerWebExchange exchange,
            ShenyuPluginChain chain,
            SelectorData selector,
            RuleData rule);

    // === Hook Methods for Customization ===

    /**
     * Handle scenarios where no route is matched.
     * Subclasses can override for custom behavior.
     */
    protected Mono<Void> handleNoMatch(PluginExecutionContext context, ShenyuPluginChain chain) {
        return chain.execute(context.getExchange());
    }

    /**
     * Handle plugin execution errors.
     * Subclasses can override for custom error handling.
     */
    protected Mono<Void> handlePluginError(
            ServerWebExchange exchange,
            ShenyuPluginChain chain,
            Throwable error) {
        // Default: continue chain execution on error
        return chain.execute(exchange);
    }

    /**
     * Create execution context from exchange.
     * Subclasses can override to add custom context data.
     */
    protected Mono<PluginExecutionContext> createExecutionContext(ServerWebExchange exchange) {
        return Mono.just(PluginExecutionContext.fromExchange(exchange));
    }

    // === Helper Methods ===

    /**
     * Check if plugin is enabled.
     */
    private Mono<Boolean> checkPluginEnabled(String pluginName) {
        return Mono.fromCallable(() -> {
            PluginData pluginData = BaseDataCache.getInstance().obtainPluginData(pluginName);
            return Objects.nonNull(pluginData) && pluginData.getEnabled();
        });
    }

    /**
     * Create default rule when selector doesn't continue to rules.
     */
    private RuleData createDefaultRule(SelectorData selector) {
        if (selector == null) {
            return null;
        }

        RuleData rule = new RuleData();
        rule.setSelectorId(selector.getId());
        rule.setPluginName(selector.getPluginName());
        rule.setId("default_rule");
        rule.setName("Default Rule");
        return rule;
    }

    /**
     * Print execution log if logging is enabled.
     */
    private void printExecutionLog(SelectorData selector, RuleData rule, String pluginName) {
        if (selector != null && selector.getLogged()) {
            LOG.info("{} selector success match, selector name: {}", pluginName, selector.getName());
        }
        if (rule != null && rule.getLoged()) {
            LOG.info("{} rule success match, rule name: {}", pluginName, rule.getName());
        }
    }

    /**
     * Lazy initialization of dependencies to avoid startup overhead.
     */
    private synchronized void initializeDependencies() {
        if (initialized) {
            return;
        }

        try {
            // Initialize RouteResolver
            this.routeResolver = getSpringBean(RouteResolver.class);

            // Initialize MetricsHelper
            this.metricsHelper = getSpringBean(MetricsHelper.class);

            initialized = true;
            LOG.debug("Dependencies initialized for plugin: {}", named());

        } catch (Exception e) {
            LOG.error("Failed to initialize dependencies for plugin: {}", named(), e);
            throw new IllegalStateException("Plugin initialization failed", e);
        }
    }

    /**
     * Get Spring bean - subclasses can override for different IoC strategies.
     */
    protected <T> T getSpringBean(Class<T> clazz) {
        return org.apache.shenyu.plugin.api.utils.SpringBeanUtils.getInstance().getBean(clazz);
    }
}