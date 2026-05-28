/*
 * AI Agent Refactoring Phase 2
 * File Format Info Tool for MCP Server
 * 
 * Copyright (c) 2026 AI Enhancement
 */

package it.unitn.ing.rista.agent.tools;

import it.unitn.ing.rista.agent.mcp.ToolDefinition;
import it.unitn.ing.rista.agent.io.DataFileServiceImpl;

import java.util.HashMap;
import java.util.Map;

/**
 * FileFormatInfoTool: MCP tool for querying supported file formats
 * 
 * Provides AI agents with information about supported data file formats.
 * 
 * @author AI Agent Refactoring
 * @version 1.0
 */
public class FileFormatInfoTool extends ToolDefinition {
    
    private final DataFileServiceImpl fileService;
    
    public FileFormatInfoTool() {
        super("get_file_format_info", "Get information about a supported file format");
        this.fileService = DataFileServiceImpl.getInstance();
        
        addParameter("format", "string", "Format name (mca, cif, dat, xye, raw)", true);
    }
    
    @Override
    public Map<String, Object> execute(Map<String, Object> args) throws Exception {
        Map<String, Object> result = new HashMap<>();
        
        String format = (String) args.get("format");
        
        if (format == null || format.isEmpty()) {
            result.put("error", "format parameter is required");
            return result;
        }
        
        return fileService.getFormatInfo(format);
    }
}
