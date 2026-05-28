/*
 * AI Agent Refactoring Phase 2
 * Data File Service Interface for Agent-Friendly I/O
 * 
 * Copyright (c) 2026 AI Enhancement
 */

package it.unitn.ing.rista.agent.api;

import java.io.File;
import java.util.List;
import java.util.Map;

/**
 * IDataFileService: Agent-friendly interface for data file operations
 * 
 * Provides abstraction for reading/writing various diffraction data formats:
 * - MCA (Multi-Channel Analyzer)
 * - CIF (Crystallographic Information File)
 * - DAT (Generic data format)
 * - XYE (X-Y-Error format)
 * 
 * @author AI Agent Refactoring
 * @version 1.0
 */
public interface IDataFileService {
    
    /**
     * List supported file formats
     * @return list of supported format extensions
     */
    List<String> getSupportedFormats();
    
    /**
     * Load a data file
     * @param filePath path to the file
     * @return parsed data as map with metadata
     */
    Map<String, Object> loadDataFile(String filePath);
    
    /**
     * Load a data file with specific format
     * @param filePath path to the file
     * @param format file format (e.g., "mca", "cif", "dat")
     * @return parsed data as map with metadata
     */
    Map<String, Object> loadDataFile(String filePath, String format);
    
    /**
     * Save data to file
     * @param filePath destination file path
     * @param data data to save
     * @param format file format
     * @return true if save successful
     */
    boolean saveDataFile(String filePath, Map<String, Object> data, String format);
    
    /**
     * Get file format info
     * @param format format name
     * @return format metadata (description, extension, etc.)
     */
    Map<String, Object> getFormatInfo(String format);
    
    /**
     * Detect file format from file extension
     * @param file file to analyze
     * @return detected format or null if unknown
     */
    String detectFormat(File file);
    
    /**
     * Validate file before loading
     * @param filePath path to file
     * @return validation result with errors if any
     */
    ValidationResult validateFile(String filePath);
    
    /**
     * Get file metadata without loading full content
     * @param filePath path to file
     * @return metadata map
     */
    Map<String, Object> getFileMetadata(String filePath);
    
    /**
     * Convert between formats
     * @param sourcePath source file path
     * @param destPath destination file path
     * @param targetFormat target format
     * @return conversion success status
     */
    boolean convertFormat(String sourcePath, String destPath, String targetFormat);
    
    /**
     * Get data statistics for loaded file
     * @param filePath path to file
     * @return statistics map (min, max, mean, std, count, etc.)
     */
    Map<String, Object> getDataStatistics(String filePath);
    
    /**
     * Stream large files in chunks
     * @param filePath path to file
     * @param chunkSize size of each chunk
     * @return list of data chunks
     */
    List<Map<String, Object>> streamDataInChunks(String filePath, int chunkSize);
    
    /**
     * Validation result container
     */
    class ValidationResult {
        public final boolean isValid;
        public final List<String> errors;
        public final List<String> warnings;
        
        public ValidationResult(boolean isValid, List<String> errors, List<String> warnings) {
            this.isValid = isValid;
            this.errors = errors;
            this.warnings = warnings;
        }
    }
}
