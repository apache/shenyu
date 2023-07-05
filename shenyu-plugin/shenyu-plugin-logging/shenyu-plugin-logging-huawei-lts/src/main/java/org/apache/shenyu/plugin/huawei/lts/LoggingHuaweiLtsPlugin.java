package org.apache.shenyu.plugin.huawei.lts;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.huawei.lts.collector.HuaweiLtsLogCollector;
import org.apache.shenyu.plugin.logging.common.AbstractLoggingPlugin;
import org.apache.shenyu.plugin.logging.common.collector.LogCollector;
import org.apache.shenyu.plugin.logging.common.entity.ShenyuRequestLog;
import org.springframework.web.server.ServerWebExchange;

public class LoggingHuaweiLtsPlugin extends AbstractLoggingPlugin<ShenyuRequestLog> {
    @Override
    protected LogCollector<ShenyuRequestLog> logCollector() {
        return HuaweiLtsLogCollector.getInstance();
    }

    @Override
    protected PluginEnum pluginEnum() {
        return PluginEnum.LOGGING_HUAWEI_LTS;
    }

    @Override
    protected ShenyuRequestLog doLogExecute(ServerWebExchange exchange, SelectorData selector, RuleData rule) {
        return new ShenyuRequestLog();
    }
}
