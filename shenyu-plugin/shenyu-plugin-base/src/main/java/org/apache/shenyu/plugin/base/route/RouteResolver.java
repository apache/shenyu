package org.apache.shenyu.plugin.base.route;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.base.context.PluginExecutionContext;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Optional;

/**
 * Route resolver interface for selector and rule matching.
 *
 * This component is responsible for all routing logic that was previously
 * embedded in AbstractShenyuPlugin.execute() method.
 */
public interface RouteResolver {

    /**
     * Match selector for the given context.
     *
     * @param context plugin execution context containing request information
     * @param selectors available selectors for the plugin
     * @return matched selector wrapped in Optional
     */
    Mono<Optional<SelectorData>> matchSelector(
            PluginExecutionContext context,
            List<SelectorData> selectors);

    /**
     * Match rule for the given context and selector.
     *
     * @param context plugin execution context
     * @param selector matched selector
     * @param rules available rules for the selector
     * @return matched rule wrapped in Optional
     */
    Mono<Optional<RuleData>> matchRule(
            PluginExecutionContext context,
            SelectorData selector,
            List<RuleData> rules);

    /**
     * Check if the plugin is applicable for the current request.
     *
     * @param context plugin execution context
     * @param pluginName name of the plugin
     * @return true if plugin should be executed
     */
    Mono<Boolean> isPluginApplicable(
            PluginExecutionContext context,
            String pluginName);

    /**
     * Resolve complete routing information for plugin execution.
     *
     * This method combines selector and rule matching logic.
     *
     * @param context plugin execution context
     * @param pluginName name of the plugin
     * @return routing result containing selector and rule data
     */
    Mono<RouteResult> resolveRoute(
            PluginExecutionContext context,
            String pluginName);
}