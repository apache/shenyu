package org.apache.shenyu.plugin.base.metrics;

import org.apache.shenyu.plugin.base.route.RouteResult;

/**
 * Metrics helper for collecting plugin execution statistics.
 *
 * This component provides a unified interface for collecting performance
 * and operational metrics from plugin executions.
 */
public interface MetricsHelper {

    /**
     * Record plugin execution time.
     *
     * @param pluginName name of the plugin
     * @param durationNanos execution time in nanoseconds
     */
    void recordExecutionTime(String pluginName, long durationNanos);

    /**
     * Record successful plugin execution.
     *
     * @param pluginName name of the plugin
     */
    void recordSuccess(String pluginName);

    /**
     * Record plugin execution error.
     *
     * @param pluginName name of the plugin
     * @param error the error that occurred
     */
    void recordError(String pluginName, Throwable error);

    /**
     * Record route matching result.
     *
     * @param pluginName name of the plugin
     * @param matchType type of match (cache hit, trie match, etc.)
     */
    void recordRouteMatch(String pluginName, RouteResult.MatchType matchType);

    /**
     * Record cache operation metrics.
     *
     * @param cacheType type of cache (selector, rule, etc.)
     * @param operation operation type (hit, miss, put, etc.)
     */
    void recordCacheOperation(String cacheType, String operation);

    /**
     * Get current metrics snapshot for a plugin.
     *
     * @param pluginName name of the plugin
     * @return metrics snapshot
     */
    PluginMetrics getPluginMetrics(String pluginName);

    /**
     * Plugin metrics data structure.
     */
    class PluginMetrics {
        private final String pluginName;
        private final long totalExecutions;
        private final long successfulExecutions;
        private final long failedExecutions;
        private final double averageExecutionTime;
        private final double successRate;

        public PluginMetrics(String pluginName, long totalExecutions,
                           long successfulExecutions, long failedExecutions,
                           double averageExecutionTime) {
            this.pluginName = pluginName;
            this.totalExecutions = totalExecutions;
            this.successfulExecutions = successfulExecutions;
            this.failedExecutions = failedExecutions;
            this.averageExecutionTime = averageExecutionTime;
            this.successRate = totalExecutions > 0 ?
                (double) successfulExecutions / totalExecutions : 0.0;
        }

        // Getters
        public String getPluginName() { return pluginName; }
        public long getTotalExecutions() { return totalExecutions; }
        public long getSuccessfulExecutions() { return successfulExecutions; }
        public long getFailedExecutions() { return failedExecutions; }
        public double getAverageExecutionTime() { return averageExecutionTime; }
        public double getSuccessRate() { return successRate; }

        @Override
        public String toString() {
            return String.format("PluginMetrics{plugin=%s, executions=%d, " +
                            "successRate=%.2f%%, avgTime=%.2fms}",
                    pluginName, totalExecutions, successRate * 100, averageExecutionTime / 1_000_000.0);
        }
    }
}