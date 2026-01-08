package org.apache.shenyu.plugin.base.metrics;

import org.apache.shenyu.plugin.base.route.RouteResult;

/**
 * No-op implementation of MetricsHelper.
 *
 * This implementation is used when metrics collection is disabled.
 * All methods are no-ops and have minimal overhead.
 */
public class NoOpMetricsHelper implements MetricsHelper {

    @Override
    public void recordExecutionTime(String pluginName, long durationNanos) {
        // No-op
    }

    @Override
    public void recordSuccess(String pluginName) {
        // No-op
    }

    @Override
    public void recordError(String pluginName, Throwable error) {
        // No-op
    }

    @Override
    public void recordRouteMatch(String pluginName, RouteResult.MatchType matchType) {
        // No-op
    }

    @Override
    public void recordCacheOperation(String cacheType, String operation) {
        // No-op
    }

    @Override
    public PluginMetrics getPluginMetrics(String pluginName) {
        return new PluginMetrics(pluginName, 0, 0, 0, 0.0);
    }
}
