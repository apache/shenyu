package org.apache.shenyu.plugin.huawei.lts.handler;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.huawei.lts.client.HuaweiLtsLogCollectClient;
import org.apache.shenyu.plugin.huawei.lts.collector.HuaweiLtsLogCollector;
import org.apache.shenyu.plugin.huawei.lts.config.HuaweiLogCollectConfig;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.config.GenericApiConfig;
import org.apache.shenyu.plugin.logging.common.handler.AbstractLogPluginDataHandler;

public class LoggingHuaweiLtsPluginDataHandler extends AbstractLogPluginDataHandler<HuaweiLogCollectConfig.HuaweiLtsLogConfig, GenericApiConfig> {

    private static final HuaweiLtsLogCollectClient HUAWEI_LTS_LOG_COLLECT_CLIENT = new HuaweiLtsLogCollectClient();

    @Override
    public String pluginNamed() {
        return PluginEnum.LOGGING_HUAWEI_LTS.getName();
    }

    @Override
    protected LogCollector logCollector() {
        return HuaweiLtsLogCollector.getInstance();
    }

    @Override
    protected void doRefreshConfig(HuaweiLogCollectConfig.HuaweiLtsLogConfig globalLogConfig) {
        HuaweiLogCollectConfig.INSTANCE.setHuaweiLtsLogConfig(globalLogConfig);
        HUAWEI_LTS_LOG_COLLECT_CLIENT.initClient(globalLogConfig);
    }

    /**
     * get Tencent log collect client.
     * @return Tencent cls log collect client.
     */
    public static HuaweiLtsLogCollectClient getHuaweiLtsLogCollectClient() {
        return HUAWEI_LTS_LOG_COLLECT_CLIENT;
    }
}
