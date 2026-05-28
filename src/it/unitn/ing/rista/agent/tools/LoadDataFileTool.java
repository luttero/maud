/*
 * AI Agent Refactoring Phase 2
 * Data File Tools for MCP Server
 * 
 * Copyright (c) 2026 AI Enhancement
 */

package it.unitn.ing.rista.agent.tools;

import it.unitn.ing.rista.agent.mcp.ToolDefinition;
import it.unitn.ing.rista.agent.io.DataFileServiceImpl;

import java.util.HashMap;
import java.util.Map;
import java.util.List;

/**
 * LoadDataFileTool: MCP tool for loading diffraction data files
 * 
 * Provides AI agents with interface to load and parse various data formats.
 * 
 * @author AI Agent Refactoring
 * @version 1.0
 */
public class LoadDataFileTool extends ToolDefinition {
    
    private final DataFileServiceImpl fileService;
    
    public LoadDataFileTool() {
        super("load_data_file", "Load a diffraction data file with automatic format detection");
        this.fileService = DataFileServiceImpl.getInstance();
        
        addParameter("filePath", "string", "Path to the data file to load", true);
        addParameter("format", "string", "File format (mca, cif, dat, xye). Auto-detect if not specified", false);
    }
    
    @Override
    public Map<String, Object> execute(Map<String, Object> args) throws Exception {
        Map<String, Object> result = new HashMap<>();
        
        String filePath = (String) args.get("filePath");
        String format = (String) args.get("format");
        
        if (filePath == null || filePath.isEmpty()) {
            result.put("error", "filePath parameter is required");
            return result;
        }
        
        if (format != null && !format.isEmpty()) {
            return fileService.loadDataFile(filePath, format);
        } else {
            return fileService.loadDataFile(filePath);
        }
    }
}
