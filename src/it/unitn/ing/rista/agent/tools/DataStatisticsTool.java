/*
 * AI Agent Refactoring Phase 2
 * Data Statistics Tool for MCP Server
 * 
 * Copyright (c) 2026 AI Enhancement
 */

package it.unitn.ing.rista.agent.tools;

import it.unitn.ing.rista.agent.mcp.ToolDefinition;
import it.unitn.ing.rista.agent.io.DataFileServiceImpl;

import java.util.HashMap;
import java.util.Map;

/**
 * DataStatisticsTool: MCP tool for extracting data statistics
 * 
 * Provides AI agents with statistical analysis of loaded data files.
 * 
 * @author AI Agent Refactoring
 * @version 1.0
 */
public class DataStatisticsTool extends ToolDefinition {
    
    private final DataFileServiceImpl fileService;
    
    public DataStatisticsTool() {
        super("get_data_statistics", "Calculate statistics for a data file (min, max, mean, count)");
        this.fileService = DataFileServiceImpl.getInstance();
        
        addParameter("filePath", "string", "Path to the data file", true);
    }
    
    @Override
    public Map<String, Object> execute(Map<String, Object> args) throws Exception {
        Map<String, Object> result = new HashMap<>();
        
        String filePath = (String) args.get("filePath");
        
        if (filePath == null || filePath.isEmpty()) {
            result.put("error", "filePath parameter is required");
            return result;
        }
        
        return fileService.getDataStatistics(filePath);
    }
}
