/*
 * AI Agent Refactoring Phase 1
 * Tool Definition for MCP Server
 * 
 * Copyright (c) 2026 AI Enhancement
 */

package it.unitn.ing.rista.agent.mcp;

import java.util.HashMap;
import java.util.Map;

/**
 * ToolDefinition: Base class for defining tools in MCP server
 * 
 * @author AI Agent Refactoring
 * @version 1.0
 */
public abstract class ToolDefinition {
    
    protected final String name;
    protected final String description;
    protected final Map<String, ParameterSpec> parameters;
    
    public ToolDefinition(String name, String description) {
        this.name = name;
        this.description = description;
        this.parameters = new HashMap<>();
    }
    
    /**
     * Add a parameter specification
     */
    protected void addParameter(String paramName, String type, String description, boolean required) {
        parameters.put(paramName, new ParameterSpec(paramName, type, description, required));
    }
    
    /**
     * Execute the tool with given arguments
     */
    public abstract Map<String, Object> execute(Map<String, Object> args) throws Exception;
    
    public String getName() {
        return name;
    }
    
    public String getDescription() {
        return description;
    }
    
    public Map<String, ParameterSpec> getParameters() {
        return new HashMap<>(parameters);
    }
    
    /**
     * ParameterSpec: Defines a tool parameter
     */
    public static class ParameterSpec {
        public final String name;
        public final String type;
        public final String description;
        public final boolean required;
        
        public ParameterSpec(String name, String type, String description, boolean required) {
            this.name = name;
            this.type = type;
            this.description = description;
            this.required = required;
        }
    }
}
