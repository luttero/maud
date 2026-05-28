/*
 * AI Agent Refactoring Phase 1
 * MCP (Model Context Protocol) Server Foundation
 * 
 * Copyright (c) 2026 AI Enhancement
 */

package it.unitn.ing.rista.agent.mcp;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * MCPServer: Foundation for Model Context Protocol integration
 * 
 * Provides base MCP server implementation for agent communication.
 * Supports tool definition and invocation for AI agents.
 * 
 * @author AI Agent Refactoring
 * @version 1.0
 */
public class MCPServer {
    
    private static final Logger LOGGER = Logger.getLogger(MCPServer.class.getName());
    private final Map<String, ToolDefinition> tools;
    private final String serverName;
    private final String serverVersion;
    private boolean isRunning;
    
    public MCPServer(String serverName, String serverVersion) {
        this.serverName = serverName;
        this.serverVersion = serverVersion;
        this.tools = new HashMap<>();
        this.isRunning = false;
        LOGGER.info("MCPServer initialized: " + serverName + " v" + serverVersion);
    }
    
    /**
     * Register a tool with the MCP server
     */
    public void registerTool(ToolDefinition tool) {
        tools.put(tool.getName(), tool);
        LOGGER.info("Tool registered: " + tool.getName());
    }
    
    /**
     * Start the MCP server
     */
    public void start() {
        if (!isRunning) {
            isRunning = true;
            LOGGER.info("MCPServer started");
        }
    }
    
    /**
     * Stop the MCP server
     */
    public void stop() {
        if (isRunning) {
            isRunning = false;
            LOGGER.info("MCPServer stopped");
        }
    }
    
    /**
     * Execute a tool
     */
    public Map<String, Object> executeTool(String toolName, Map<String, Object> args) {
        ToolDefinition tool = tools.get(toolName);
        if (tool == null) {
            Map<String, Object> error = new HashMap<>();
            error.put("error", "Tool not found: " + toolName);
            return error;
        }
        
        try {
            return tool.execute(args);
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Tool execution failed: " + toolName, e);
            Map<String, Object> error = new HashMap<>();
            error.put("error", e.getMessage());
            return error;
        }
    }
    
    /**
     * Get list of available tools
     */
    public Map<String, ToolDefinition> getAvailableTools() {
        return new HashMap<>(tools);
    }
    
    public String getServerName() {
        return serverName;
    }
    
    public String getServerVersion() {
        return serverVersion;
    }
    
    public boolean isRunning() {
        return isRunning;
    }
}
