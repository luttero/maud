/*
 * AI Agent Refactoring Phase 1
 * Preferences Service Interface for Agent-Friendly API
 * 
 * Copyright (c) 2026 AI Enhancement
 */

package it.unitn.ing.rista.agent.api;

import java.util.Map;
import java.util.Optional;

/**
 * IPreferencesService: Agent-friendly preferences management interface
 * 
 * Provides clean API for AI agents to interact with application configuration.
 * Supports type-safe operations and batch transactions.
 * 
 * @author AI Agent Refactoring
 * @version 1.0
 */
public interface IPreferencesService {
    
    /**
     * Retrieve a string preference
     * @param key preference key
     * @param defaultValue fallback value if not found
     * @return preference value or default
     */
    String getString(String key, String defaultValue);
    
    /**
     * Retrieve an integer preference
     * @param key preference key
     * @param defaultValue fallback value if not found
     * @return preference value or default
     */
    int getInt(String key, int defaultValue);
    
    /**
     * Retrieve a double preference
     * @param key preference key
     * @param defaultValue fallback value if not found
     * @return preference value or default
     */
    double getDouble(String key, double defaultValue);
    
    /**
     * Retrieve a long preference
     * @param key preference key
     * @param defaultValue fallback value if not found
     * @return preference value or default
     */
    long getLong(String key, long defaultValue);
    
    /**
     * Retrieve a boolean preference
     * @param key preference key
     * @param defaultValue fallback value if not found
     * @return preference value or default
     */
    boolean getBoolean(String key, boolean defaultValue);
    
    /**
     * Set a string preference
     * @param key preference key
     * @param value preference value
     */
    void setString(String key, String value);
    
    /**
     * Set an integer preference
     * @param key preference key
     * @param value preference value
     */
    void setInt(String key, int value);
    
    /**
     * Set a double preference
     * @param key preference key
     * @param value preference value
     */
    void setDouble(String key, double value);
    
    /**
     * Set a long preference
     * @param key preference key
     * @param value preference value
     */
    void setLong(String key, long value);
    
    /**
     * Set a boolean preference
     * @param key preference key
     * @param value preference value
     */
    void setBoolean(String key, boolean value);
    
    /**
     * Check if preference exists
     * @param key preference key
     * @return true if preference exists
     */
    boolean exists(String key);
    
    /**
     * Remove a preference
     * @param key preference key
     */
    void remove(String key);
    
    /**
     * Get all preferences as a map
     * @return map of all preference keys and values
     */
    Map<String, Object> getAll();
    
    /**
     * Get preferences for a specific category/namespace
     * @param namespace namespace/category prefix
     * @return map of preferences in namespace
     */
    Map<String, Object> getByNamespace(String namespace);
    
    /**
     * Clear all preferences
     */
    void clear();
    
    /**
     * Reset preferences to defaults
     */
    void resetToDefaults();
    
    /**
     * Export preferences as JSON string
     * @return JSON representation of all preferences
     */
    String exportAsJson();
    
    /**
     * Import preferences from JSON string
     * @param jsonData JSON string with preference data
     */
    void importFromJson(String jsonData);
    
    /**
     * Start transaction for batch operations
     * @return transaction handle
     */
    IPreferenceTransaction beginTransaction();
}
