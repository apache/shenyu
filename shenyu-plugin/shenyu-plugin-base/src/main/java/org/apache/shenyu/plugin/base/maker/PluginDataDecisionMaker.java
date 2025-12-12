package org.apache.shenyu.plugin.base.maker;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.ShenyuPluginChain;
import org.apache.shenyu.plugin.base.provider.PluginDataProvider;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.util.List;

/**
 * A specialized decision maker implementation responsible for plugin data matching and decision-making
 * within the Apache ShenYu gateway ecosystem.
 *
 * <p>This class extends {@link AbstractMatchDecisionMaker} to provide plugin-specific matching
 * capabilities for {@link PluginData} objects. It serves as the foundational decision maker in
 * ShenYu's three-tier matching system (Plugin → Selector → Rule), determining which plugins should
 * process incoming requests based on their enabled status and configuration data.</p>
 *
 * <p>In the ShenYu gateway architecture, this component operates at the highest level of the
 * decision hierarchy:
 * <ol>
 *   <li><strong>Plugin Level</strong>: Determines if a plugin is enabled and should process the request</li>
 *   <li><strong>Selector Level</strong>: Handles coarse-grained routing decisions</li>
 *   <li><strong>Rule Level</strong>: Applies fine-grained processing rules</li>
 * </ol>
 * The PluginDataDecisionMaker ensures that only active and properly configured plugins participate
 * in request processing, maintaining gateway efficiency and reliability.</p>
 *
 * <p>This decision maker integrates with ShenYu's plugin chain mechanism through the provided
 * {@link PluginDataProvider}, which supplies the plugin configuration data needed for matching
 * decisions. It follows the gateway's reactive programming model by returning {@link Mono} types
 * for all asynchronous operations.</p>
 */
public class PluginDataDecisionMaker extends AbstractMatchDecisionMaker<PluginData> {

    /**
     * Constructs a new PluginDataDecisionMaker with a PluginDataProvider.
     *
     * <p>This constructor initializes the decision maker with a dedicated data provider
     * that supplies plugin configuration information. The provider enables access to
     * plugin metadata including enabled status, configuration parameters, and execution
     * order settings.</p>
     */
    public PluginDataDecisionMaker() {
        super(new PluginDataProvider());
    }

    /**
     * Handles the scenario when no plugin data is found for the specified plugin.
     *
     * <p>When no matching plugin data is available, this method ensures the request
     * continues through the plugin chain without interruption. This graceful degradation
     * approach maintains system reliability by allowing requests to proceed even when
     * specific plugin configurations are missing or unavailable.</p>
     *
     * @param pluginName the name of the plugin being processed
     * @param exchange the current server web exchange containing request and response data
     * @param chain the plugin chain for continued request processing
     * @return a Mono indicating completion of the empty handler operation
     */
    @Override
    public Mono<Void> handleEmpty(String pluginName, ServerWebExchange exchange, ShenyuPluginChain chain) {
        return chain.execute(exchange);
    }

    /**
     * Matches plugin data against the current request context.
     *
     * <p>This method implements the core plugin matching logic, selecting the appropriate
     * plugin data from the available candidates. It employs a straightforward selection
     * strategy: when multiple plugin data entries are available, it returns the first one
     * from the list, assuming the list is pre-sorted by priority or relevance.</p>
     *
     * <p>In typical ShenYu usage, plugin data lists contain configuration for a specific
     * plugin name, so returning the first entry is semantically correct as there should
     * be only one active configuration per plugin.</p>
     *
     * @param exchange the current server web exchange containing request information
     * @param dataName the name of the plugin data being matched
     * @param dataList the list of candidate plugin data for matching
     * @param path the request path (currently unused in base implementation)
     * @param selectorData the selector data context (currently unused in base implementation)
     * @return the matched PluginData, or null if the data list is empty
     */
    @Override
    public PluginData matchData(ServerWebExchange exchange, String dataName, List<PluginData> dataList, String path, SelectorData selectorData) {
        return dataList.isEmpty() ? null : dataList.get(0);
    }

    /**
     * Determines whether the plugin chain should continue processing after the current plugin.
     *
     * <p>This decision is based solely on the plugin's enabled status. Only plugins that are
     * explicitly enabled will allow continuation of the plugin chain. This provides a simple
     * but effective mechanism for controlling plugin execution flow within the gateway.</p>
     *
     * <p>The continuation mechanism enables complex processing scenarios where certain plugins
     * might terminate request processing (e.g., authentication failures) while others allow
     * progression to subsequent processing stages.</p>
     *
     * @param data the plugin data to evaluate for continuation
     * @return true if the plugin is enabled and should continue processing, false otherwise
     */
    @Override
    public boolean shouldContinue(PluginData data) {
        return data != null && data.getEnabled();
    }
}