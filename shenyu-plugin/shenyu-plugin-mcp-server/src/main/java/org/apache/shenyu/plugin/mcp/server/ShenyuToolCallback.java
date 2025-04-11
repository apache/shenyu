package org.apache.shenyu.plugin.mcp.server;

import org.springframework.ai.chat.model.ToolContext;
import org.springframework.ai.tool.ToolCallback;
import org.springframework.ai.tool.definition.ToolDefinition;

public class ShenyuToolCallback implements ToolCallback {
    
    private final ToolDefinition toolDefinition;
    
    public ShenyuToolCallback(final ToolDefinition toolDefinition) {
        this.toolDefinition = toolDefinition;
    }
    
    @Override
    public ToolDefinition getToolDefinition() {
        return this.toolDefinition;
    }
    
    @Override
    public String call(final String toolInput) {
        return toolInput;
    }
    
    @Override
    public String call(final String toolInput, final ToolContext tooContext) {
        return this.call(toolInput);
    }
}
