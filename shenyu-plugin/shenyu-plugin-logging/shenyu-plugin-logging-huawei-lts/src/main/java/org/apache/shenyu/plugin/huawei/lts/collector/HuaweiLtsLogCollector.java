package org.apache.shenyu.plugin.huawei.lts.collector;

import org.apache.shenyu.plugin.huawei.lts.LoggingHuaweiLtsPlugin;
import org.apache.shenyu.plugin.huawei.lts.client.HuaweiLtsLogCollectClient;
import org.apache.shenyu.plugin.huawei.lts.handler.LoggingHuaweiLtsPluginDataHandler;
import org.apache.shenyu.plugin.logging.common.collector.AbstractLogCollector;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.apache.shenyu.plugin.logging.desensitize.api.matcher.KeyWordMatch;

public class HuaweiLtsLogCollector extends AbstractLogCollector<HuaweiLtsLogCollectClient, ShenyuRequestLog> {


    private static final LogCollector<ShenyuRequestLog> INSTANCE = new HuaweiLtsLogCollector();

    /**
     * get LogCollector instance.
     *
     * @return LogCollector instance
     */
    public static LogCollector<ShenyuRequestLog> getInstance() {
        return INSTANCE;
    }

    @Override
    protected HuaweiLtsLogCollectClient getLogConsumeClient() {
        return LoggingHuaweiLtsPluginDataHandler.getHuaweiLtsLogCollectClient();
    }

    @Override
    protected void desensitizeLog(ShenyuRequestLog log, KeyWordMatch keyWordMatch, String desensitizeAlg) {

    }
}
