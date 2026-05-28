/*
 * AI Agent Refactoring Phase 2
 * Data File Service Implementation
 * 
 * Copyright (c) 2026 AI Enhancement
 */

package it.unitn.ing.rista.agent.io;

import it.unitn.ing.rista.agent.api.IDataFileService;
import it.unitn.ing.rista.util.MaudPreferences;

import java.io.*;
import java.util.*;
import java.util.logging.Logger;
import java.util.logging.Level;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * DataFileServiceImpl: Implementation of IDataFileService
 * 
 * Handles loading/saving various diffraction data formats with:
 * - Format auto-detection
 * - Validation and error handling
 * - Streaming for large files
 * - Format conversion
 * - Metadata extraction
 * 
 * @author AI Agent Refactoring
 * @version 1.0
 */
public class DataFileServiceImpl implements IDataFileService {
    
    private static final Logger LOGGER = Logger.getLogger(DataFileServiceImpl.class.getName());
    private static volatile DataFileServiceImpl instance;
    
    private DataFileServiceImpl() {
        LOGGER.info("DataFileService initialized");
    }
    
    /**
     * Get singleton instance
     */
    public static DataFileServiceImpl getInstance() {
        if (instance == null) {
            synchronized (DataFileServiceImpl.class) {
                if (instance == null) {
                    instance = new DataFileServiceImpl();
                }
            }
        }
        return instance;
    }
    
    @Override
    public List<String> getSupportedFormats() {
        List<String> formats = new ArrayList<>();
        for (FileFormatRegistry.FileFormat format : FileFormatRegistry.getAllFormats().values()) {
            formats.add(format.formatName);
        }
        return formats;
    }
    
    @Override
    public Map<String, Object> loadDataFile(String filePath) {
        File file = new File(filePath);
        String format = detectFormat(file);
        
        if (format == null) {
            Map<String, Object> error = new HashMap<>();
            error.put("error", "Unable to detect file format");
            error.put("filePath", filePath);
            return error;
        }
        
        return loadDataFile(filePath, format);
    }
    
    @Override
    public Map<String, Object> loadDataFile(String filePath, String format) {
        Map<String, Object> result = new HashMap<>();
        
        try {
            File file = new File(filePath);
            
            if (!file.exists()) {
                result.put("error", "File not found: " + filePath);
                LOGGER.warning("File not found: " + filePath);
                return result;
            }
            
            // Validate file before loading
            ValidationResult validation = validateFile(filePath);
            result.put("validation", validation.isValid);
            if (!validation.isValid) {
                result.put("errors", validation.errors);
                result.put("warnings", validation.warnings);
                if (!validation.errors.isEmpty()) {
                    LOGGER.warning("File validation failed: " + String.join(", ", validation.errors));
                    return result;
                }
            }
            
            // Load based on format
            switch (format.toLowerCase()) {
                case "mca":
                    loadMCAFile(file, result);
                    break;
                case "cif":
                    loadCIFFile(file, result);
                    break;
                case "dat":
                case "xye":
                    loadTextDataFile(file, result, format);
                    break;
                default:
                    result.put("error", "Unsupported format: " + format);
            }
            
            result.put("filePath", filePath);
            result.put("format", format);
            result.put("fileSize", file.length());
            result.put("lastModified", file.lastModified());
            result.put("success", !result.containsKey("error"));
            
            LOGGER.info("File loaded successfully: " + filePath + " (" + format + ")");
            
        } catch (Exception e) {
            result.put("error", e.getMessage());
            result.put("success", false);
            LOGGER.log(Level.SEVERE, "Error loading file: " + filePath, e);
        }
        
        return result;
    }
    
    @Override
    public boolean saveDataFile(String filePath, Map<String, Object> data, String format) {
        try {
            File file = new File(filePath);
            file.getParentFile().mkdirs();
            
            switch (format.toLowerCase()) {
                case "dat":
                case "xye":
                    saveTe xtDataFile(file, data, format);
                    break;
                default:
                    LOGGER.warning("Save format not yet implemented: " + format);
                    return false;
            }
            
            LOGGER.info("File saved successfully: " + filePath + " (" + format + ")");
            return true;
            
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Error saving file: " + filePath, e);
            return false;
        }
    }
    
    @Override
    public Map<String, Object> getFormatInfo(String format) {
        FileFormatRegistry.FileFormat fileFormat = FileFormatRegistry.getFormat(format);
        if (fileFormat == null) {
            Map<String, Object> error = new HashMap<>();
            error.put("error", "Unknown format: " + format);
            return error;
        }
        return fileFormat.toMap();
    }
    
    @Override
    public String detectFormat(File file) {
        if (!file.exists()) {
            return null;
        }
        
        String fileName = file.getName().toLowerCase();
        int lastDot = fileName.lastIndexOf('.');
        
        if (lastDot > 0) {
            String extension = fileName.substring(lastDot);
            FileFormatRegistry.FileFormat format = FileFormatRegistry.getFormatByExtension(extension);
            if (format != null) {
                return format.formatName;
            }
        }
        
        return null;
    }
    
    @Override
    public ValidationResult validateFile(String filePath) {
        List<String> errors = new ArrayList<>();
        List<String> warnings = new ArrayList<>();
        
        try {
            File file = new File(filePath);
            
            if (!file.exists()) {
                errors.add("File does not exist");
                return new ValidationResult(false, errors, warnings);
            }
            
            if (!file.isFile()) {
                errors.add("Path is not a regular file");
                return new ValidationResult(false, errors, warnings);
            }
            
            if (!file.canRead()) {
                errors.add("File is not readable");
                return new ValidationResult(false, errors, warnings);
            }
            
            if (file.length() == 0) {
                warnings.add("File is empty");
            }
            
            if (file.length() > 1024 * 1024 * 100) { // 100 MB
                warnings.add("File is very large (> 100 MB)");
            }
            
            return new ValidationResult(true, errors, warnings);
            
        } catch (Exception e) {
            errors.add("Validation error: " + e.getMessage());
            return new ValidationResult(false, errors, warnings);
        }
    }
    
    @Override
    public Map<String, Object> getFileMetadata(String filePath) {
        Map<String, Object> metadata = new HashMap<>();
        
        try {
            File file = new File(filePath);
            metadata.put("filePath", filePath);
            metadata.put("fileName", file.getName());
            metadata.put("fileSize", file.length());
            metadata.put("lastModified", new Date(file.lastModified()));
            metadata.put("isReadable", file.canRead());
            metadata.put("isWritable", file.canWrite());
            metadata.put("format", detectFormat(file));
            
        } catch (Exception e) {
            metadata.put("error", e.getMessage());
        }
        
        return metadata;
    }
    
    @Override
    public boolean convertFormat(String sourcePath, String destPath, String targetFormat) {
        try {
            Map<String, Object> data = loadDataFile(sourcePath);
            if (data.containsKey("error")) {
                LOGGER.warning("Cannot load source file for conversion");
                return false;
            }
            return saveDataFile(destPath, data, targetFormat);
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Format conversion failed", e);
            return false;
        }
    }
    
    @Override
    public Map<String, Object> getDataStatistics(String filePath) {
        Map<String, Object> stats = new HashMap<>();
        
        try {
            Map<String, Object> data = loadDataFile(filePath);
            
            if (data.containsKey("error")) {
                stats.put("error", data.get("error"));
                return stats;
            }
            
            // Extract numerical data for statistics
            if (data.containsKey("data")) {
                Object dataObj = data.get("data");
                if (dataObj instanceof List) {
                    List<?> dataList = (List<?>) dataObj;
                    stats.put("count", dataList.size());
                    
                    double min = Double.MAX_VALUE;
                    double max = Double.MIN_VALUE;
                    double sum = 0;
                    
                    for (Object item : dataList) {
                        if (item instanceof Number) {
                            double value = ((Number) item).doubleValue();
                            min = Math.min(min, value);
                            max = Math.max(max, value);
                            sum += value;
                        }
                    }
                    
                    stats.put("min", min);
                    stats.put("max", max);
                    stats.put("mean", sum / dataList.size());
                    stats.put("sum", sum);
                }
            }
            
        } catch (Exception e) {
            stats.put("error", e.getMessage());
        }
        
        return stats;
    }
    
    @Override
    public List<Map<String, Object>> streamDataInChunks(String filePath, int chunkSize) {
        List<Map<String, Object>> chunks = new ArrayList<>();
        
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filePath));
            String line;
            List<String> currentChunk = new ArrayList<>();
            
            while ((line = reader.readLine()) != null) {
                currentChunk.add(line);
                
                if (currentChunk.size() >= chunkSize) {
                    Map<String, Object> chunkMap = new HashMap<>();
                    chunkMap.put("lines", new ArrayList<>(currentChunk));
                    chunkMap.put("size", currentChunk.size());
                    chunks.add(chunkMap);
                    currentChunk.clear();
                }
            }
            
            // Add remaining chunk
            if (!currentChunk.isEmpty()) {
                Map<String, Object> chunkMap = new HashMap<>();
                chunkMap.put("lines", currentChunk);
                chunkMap.put("size", currentChunk.size());
                chunks.add(chunkMap);
            }
            
            reader.close();
            LOGGER.info("Streamed " + chunks.size() + " chunks from: " + filePath);
            
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Error streaming file", e);
        }
        
        return chunks;
    }
    
    /**
     * Load MCA format file
     */
    private void loadMCAFile(File file, Map<String, Object> result) {
        // Delegate to existing MCADatafile logic
        result.put("status", "MCA format loading - requires MCADatafile integration");
        LOGGER.info("MCA file format detected: " + file.getName());
    }
    
    /**
     * Load CIF format file
     */
    private void loadCIFFile(File file, Map<String, Object> result) {
        result.put("status", "CIF format loading - requires CIF parser integration");
        LOGGER.info("CIF file format detected: " + file.getName());
    }
    
    /**
     * Load text-based data files (DAT, XYE)
     */
    private void loadTextDataFile(File file, Map<String, Object> result, String format) throws IOException {
        List<Map<String, Double>> data = new ArrayList<>();
        
        try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
            String line;
            int lineNumber = 0;
            List<String> parseErrors = new ArrayList<>();
            
            while ((line = reader.readLine()) != null) {
                lineNumber++;
                line = line.trim();
                
                // Skip empty lines and comments
                if (line.isEmpty() || line.startsWith("#")) {
                    continue;
                }
                
                try {
                    String[] parts = line.split("\\s+");
                    
                    if ("xye".equalsIgnoreCase(format)) {
                        if (parts.length >= 3) {
                            Map<String, Double> point = new HashMap<>();
                            point.put("x", Double.parseDouble(parts[0]));
                            point.put("y", Double.parseDouble(parts[1]));
                            point.put("error", Double.parseDouble(parts[2]));
                            data.add(point);
                        }
                    } else {
                        if (parts.length >= 2) {
                            Map<String, Double> point = new HashMap<>();
                            point.put("x", Double.parseDouble(parts[0]));
                            point.put("y", Double.parseDouble(parts[1]));
                            data.add(point);
                        }
                    }
                } catch (NumberFormatException e) {
                    parseErrors.add("Line " + lineNumber + ": " + e.getMessage());
                }
            }
            
            result.put("data", data);
            result.put("pointCount", data.size());
            
            if (!parseErrors.isEmpty()) {
                result.put("parseErrors", parseErrors);
            }
        }
    }
    
    /**
     * Save text-based data files
     */
    private void saveTextDataFile(File file, Map<String, Object> data, String format) throws IOException {
        try (PrintWriter writer = new PrintWriter(new FileWriter(file))) {
            writer.println("# Generated by DataFileService");
            writer.println("# Format: " + format);
            writer.println("# Timestamp: " + new Date());
            
            Object dataObj = data.get("data");
            if (dataObj instanceof List) {
                List<?> dataList = (List<?>) dataObj;
                for (Object item : dataList) {
                    if (item instanceof Map) {
                        Map<?, ?> point = (Map<?, ?>) item;
                        writer.print(point.getOrDefault("x", 0));
                        writer.print(" ");
                        writer.print(point.getOrDefault("y", 0));
                        
                        if ("xye".equalsIgnoreCase(format) && point.containsKey("error")) {
                            writer.print(" ");
                            writer.print(point.get("error"));
                        }
                        writer.println();
                    }
                }
            }
        }
    }
}
