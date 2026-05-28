/*
 * AI Agent Refactoring Phase 1
 * Preferences Tool for MCP Server
 * 
 * Copyright (c) 2026 AI Enhancement
 */

package it.unitn.ing.rista.agent.tools;

import it.unitn.ing.rista.agent.mcp.ToolDefinition;
import it.unitn.ing.rista.agent.config.ConfigManager;

import java.util.HashMap;
import java.util.Map;

/**
 * PreferencesTool: MCP tool for preference management
 * 
 * Provides AI agents with interface to read/write application preferences.
 * 
 * @author AI Agent Refactoring
 * @version 1.0
 */
public class PreferencesTool extends ToolDefinition {
    
    private final ConfigManager configManager;
    
    public PreferencesTool() {
        super("get_preference", "Retrieve a preference value by key");
        this.configManager = ConfigManager.getInstance();
        
        addParameter("key", "string", "Preference key", true);
        addParameter("default_value", "string", "Default value if not found", false);
    }
    
    @Override
    public Map<String, Object> execute(Map<String, Object> args) throws Exception {
        Map<String, Object> result = new HashMap<>();
        
        String key = (String) args.get("key");
        String defaultValue = (String) args.getOrDefault("default_value", "");
        
        if (key == null || key.isEmpty()) {
            result.put("error", "key parameter is required");
            return result;
        }
        
        String value = configManager.getString(key, defaultValue);
        result.put("key", key);
        result.put("value", value);
        result.put("exists", configManager.exists(key));
        
        return result;
    }
}
