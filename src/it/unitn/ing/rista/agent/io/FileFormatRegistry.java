/*
 * AI Agent Refactoring Phase 2
 * Data File Format Definitions
 * 
 * Copyright (c) 2026 AI Enhancement
 */

package it.unitn.ing.rista.agent.io;

import java.util.HashMap;
import java.util.Map;

/**
 * FileFormatRegistry: Centralized registry for supported file formats
 * 
 * @author AI Agent Refactoring
 * @version 1.0
 */
public class FileFormatRegistry {
    
    private static final Map<String, FileFormat> formats = new HashMap<>();
    
    static {
        // Initialize supported formats
        registerFormat(new FileFormat(
            "mca",
            "Multi-Channel Analyzer",
            ".mca",
            "Binary format from MCA detectors (AMTEK standard)",
            true
        ));
        
        registerFormat(new FileFormat(
            "cif",
            "Crystallographic Information File",
            ".cif",
            "Text-based crystallographic data format",
            true
        ));
        
        registerFormat(new FileFormat(
            "dat",
            "Generic Data Format",
            ".dat",
            "Plain text X-Y data format",
            true
        ));
        
        registerFormat(new FileFormat(
            "xye",
            "X-Y-Error Format",
            ".xye",
            "Three-column format: X, Y, Error",
            true
        ));
        
        registerFormat(new FileFormat(
            "raw",
            "Raw Detector Data",
            ".raw",
            "Raw binary detector output",
            true
        ));
    }
    
    /**
     * Register a file format
     */
    public static void registerFormat(FileFormat format) {
        formats.put(format.formatName, format);
    }
    
    /**
     * Get format by name
     */
    public static FileFormat getFormat(String formatName) {
        return formats.get(formatName.toLowerCase());
    }
    
    /**
     * Get format by file extension
     */
    public static FileFormat getFormatByExtension(String extension) {
        for (FileFormat format : formats.values()) {
            if (format.fileExtension.equalsIgnoreCase(extension)) {
                return format;
            }
        }
        return null;
    }
    
    /**
     * Get all registered formats
     */
    public static Map<String, FileFormat> getAllFormats() {
        return new HashMap<>(formats);
    }
    
    /**
     * FileFormat: Metadata for a supported format
     */
    public static class FileFormat {
        public final String formatName;
        public final String displayName;
        public final String fileExtension;
        public final String description;
        public final boolean isReadable;
        
        public FileFormat(String formatName, String displayName, String fileExtension, 
                         String description, boolean isReadable) {
            this.formatName = formatName;
            this.displayName = displayName;
            this.fileExtension = fileExtension;
            this.description = description;
            this.isReadable = isReadable;
        }
        
        public Map<String, Object> toMap() {
            Map<String, Object> map = new HashMap<>();
            map.put("name", formatName);
            map.put("displayName", displayName);
            map.put("extension", fileExtension);
            map.put("description", description);
            map.put("readable", isReadable);
            return map;
        }
    }
}
