package org.apache.shenyu.plugin.mcp.server;

import org.apache.shenyu.common.enums.PluginEnum;
import org.springframework.ai.tool.annotation.Tool;

public class ShenyuMcpToolsProvider {
    
    @Tool(description = "返回插件列表")
    public String listAll() {
        PluginEnum[] pluginEnums = PluginEnum.values();
        return pluginEnums.toString();
    }
}
