package org.apache.shenyu.plugin.logging.pulsar.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.logging.common.constant.GenericLoggingConstant;
import org.apache.shenyu.plugin.logging.pulsar.client.PulsarLogCollectClient;
import org.apache.shenyu.plugin.logging.pulsar.collector.PulsarLogCollector;
import org.apache.shenyu.plugin.logging.pulsar.config.PulsarLogCollectConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ConcurrentHashMap;


/**
 * The type logging pulsar plugin data handler
 */
public class LoggingPulsarPluginDataHandler implements PluginDataHandler {

    private static final Logger LOG = LoggerFactory.getLogger(LoggingPulsarPluginDataHandler.class);

    private static final PulsarLogCollectClient PULSAR_LOG_COLLECT_CLIENT = new PulsarLogCollectClient();

    private static final String EMPTY_JSON = "{}";

    private static final Map<String, List<String>> SELECT_ID_URI_LIST_MAP = new ConcurrentHashMap<>();

    @Override
    public void handlerPlugin(PluginData pluginData) {
        LOG.info("handler loggingPulsar Plugin data:{}", GsonUtils.getGson().toJson(pluginData));
        if (pluginData.getEnabled()) {
            PulsarLogCollectConfig.PulsarLogConfig globalLogConfig = GsonUtils.getInstance().fromJson(pluginData.getConfig(),
                    PulsarLogCollectConfig.PulsarLogConfig.class);
            PulsarLogCollectConfig.INSTANCE.setPulsarLogConfig(globalLogConfig);
            // start pulsar producer
            Properties properties = new Properties();
            properties.setProperty(GenericLoggingConstant.TOPIC, globalLogConfig.getTopic());
            properties.setProperty(GenericLoggingConstant.SERVICE_URL, globalLogConfig.getServiceUrl());
            PULSAR_LOG_COLLECT_CLIENT.initProducer(properties);
            PulsarLogCollector.getInstance().start();
        } else {
            try {
                PulsarLogCollector.getInstance().close();
            } catch (Exception e) {
                LOG.error("close log collector error", e);
            }
        }
    }

    @Override
    public void handlerSelector(SelectorData selectorData) {
        PluginDataHandler.super.handlerSelector(selectorData);
    }

    @Override
    public void removeSelector(SelectorData selectorData) {
        PluginDataHandler.super.removeSelector(selectorData);
    }

    @Override
    public String pluginNamed() {
        return PluginEnum.LOGGING_PULSAR.getName();
    }

    public static PulsarLogCollectClient getPulsarLogCollectClient() {
        return PULSAR_LOG_COLLECT_CLIENT;
    }
}
