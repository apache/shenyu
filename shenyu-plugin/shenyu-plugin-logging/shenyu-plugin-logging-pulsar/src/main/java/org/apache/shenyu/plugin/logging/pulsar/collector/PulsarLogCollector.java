package org.apache.shenyu.plugin.logging.pulsar.collector;

import org.apache.shenyu.plugin.logging.common.client.LogConsumeClient;
import org.apache.shenyu.plugin.logging.common.collector.AbstractLogCollector;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.pulsar.handler.LoggingPulsarPluginDataHandler;

import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class PulsarLogCollector extends AbstractLogCollector {

    private static final LogCollector INSTANCE = new PulsarLogCollector();

    /**
     * get LogCollector Instance.
     *
     * @return LogCollector instance
     */
    public static LogCollector getInstance() {
        return INSTANCE;
    }
    @Override
    protected LogConsumeClient getLogConsumeClient() {
        return LoggingPulsarPluginDataHandler.getPulsarLogCollectClient();
    }
}
