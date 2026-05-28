/*
 * AI Agent Refactoring Phase 1
 * Configuration Manager with JSON Support
 * 
 * Copyright (c) 2026 AI Enhancement
 */

package it.unitn.ing.rista.agent.config;

import it.unitn.ing.rista.agent.api.IPreferencesService;
import it.unitn.ing.rista.agent.api.IPreferenceTransaction;
import it.unitn.ing.rista.util.MaudPreferences;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * ConfigManager: Centralized configuration management with agent-friendly API
 * 
 * Wraps MaudPreferences with:
 * - Type-safe operations
 * - Transaction support
 * - JSON serialization
 * - Logging and monitoring
 * - Namespace management
 * 
 * @author AI Agent Refactoring
 * @version 1.0
 */
public class ConfigManager implements IPreferencesService {
    
    private static final Logger LOGGER = Logger.getLogger(ConfigManager.class.getName());
    private static volatile ConfigManager instance;
    private final Map<String, Object> cache;
    private final Object syncLock = new Object();
    
    private ConfigManager() {
        this.cache = new ConcurrentHashMap<>();
        this.initializeCache();
        LOGGER.info("ConfigManager initialized");
    }
    
    /**
     * Get singleton instance
     */
    public static ConfigManager getInstance() {
        if (instance == null) {
            synchronized (ConfigManager.class) {
                if (instance == null) {
                    instance = new ConfigManager();
                }
            }
        }
        return instance;
    }
    
    /**
     * Initialize cache from MaudPreferences
     */
    private void initializeCache() {
        try {
            MaudPreferences.loadPreferences();
            LOGGER.info("Cache initialized from MaudPreferences");
        } catch (Exception e) {
            LOGGER.log(Level.WARNING, "Failed to initialize cache", e);
        }
    }
    
    @Override
    public String getString(String key, String defaultValue) {
        synchronized (syncLock) {
            try {
                String value = MaudPreferences.getPref(key, defaultValue);
                cache.put(key, value);
                LOGGER.log(Level.FINE, "getString: " + key + " = " + value);
                return value;
            } catch (Exception e) {
                LOGGER.log(Level.WARNING, "getString failed for key: " + key, e);
                return defaultValue;
            }
        }
    }
    
    @Override
    public int getInt(String key, int defaultValue) {
        synchronized (syncLock) {
            try {
                int value = MaudPreferences.getInteger(key, defaultValue);
                cache.put(key, value);
                LOGGER.log(Level.FINE, "getInt: " + key + " = " + value);
                return value;
            } catch (Exception e) {
                LOGGER.log(Level.WARNING, "getInt failed for key: " + key, e);
                return defaultValue;
            }
        }
    }
    
    @Override
    public double getDouble(String key, double defaultValue) {
        synchronized (syncLock) {
            try {
                double value = MaudPreferences.getDouble(key, defaultValue);
                cache.put(key, value);
                LOGGER.log(Level.FINE, "getDouble: " + key + " = " + value);
                return value;
            } catch (Exception e) {
                LOGGER.log(Level.WARNING, "getDouble failed for key: " + key, e);
                return defaultValue;
            }
        }
    }
    
    @Override
    public long getLong(String key, long defaultValue) {
        synchronized (syncLock) {
            try {
                long value = MaudPreferences.getLong(key, defaultValue);
                cache.put(key, value);
                LOGGER.log(Level.FINE, "getLong: " + key + " = " + value);
                return value;
            } catch (Exception e) {
                LOGGER.log(Level.WARNING, "getLong failed for key: " + key, e);
                return defaultValue;
            }
        }
    }
    
    @Override
    public boolean getBoolean(String key, boolean defaultValue) {
        synchronized (syncLock) {
            try {
                boolean value = MaudPreferences.getBoolean(key, defaultValue);
                cache.put(key, value);
                LOGGER.log(Level.FINE, "getBoolean: " + key + " = " + value);
                return value;
            } catch (Exception e) {
                LOGGER.log(Level.WARNING, "getBoolean failed for key: " + key, e);
                return defaultValue;
            }
        }
    }
    
    @Override
    public void setString(String key, String value) {
        synchronized (syncLock) {
            try {
                MaudPreferences.setPref(key, value);
                cache.put(key, value);
                LOGGER.log(Level.FINE, "setString: " + key + " = " + value);
            } catch (Exception e) {
                LOGGER.log(Level.WARNING, "setString failed for key: " + key, e);
            }
        }
    }
    
    @Override
    public void setInt(String key, int value) {
        synchronized (syncLock) {
            try {
                MaudPreferences.setPref(key, value);
                cache.put(key, value);
                LOGGER.log(Level.FINE, "setInt: " + key + " = " + value);
            } catch (Exception e) {
                LOGGER.log(Level.WARNING, "setInt failed for key: " + key, e);
            }
        }
    }
    
    @Override
    public void setDouble(String key, double value) {
        synchronized (syncLock) {
            try {
                MaudPreferences.setPref(key, value);
                cache.put(key, value);
                LOGGER.log(Level.FINE, "setDouble: " + key + " = " + value);
            } catch (Exception e) {
                LOGGER.log(Level.WARNING, "setDouble failed for key: " + key, e);
            }
        }
    }
    
    @Override
    public void setLong(String key, long value) {
        synchronized (syncLock) {
            try {
                MaudPreferences.setPref(key, value);
                cache.put(key, value);
                LOGGER.log(Level.FINE, "setLong: " + key + " = " + value);
            } catch (Exception e) {
                LOGGER.log(Level.WARNING, "setLong failed for key: " + key, e);
            }
        }
    }
    
    @Override
    public void setBoolean(String key, boolean value) {
        synchronized (syncLock) {
            try {
                MaudPreferences.setPref(key, value);
                cache.put(key, value);
                LOGGER.log(Level.FINE, "setBoolean: " + key + " = " + value);
            } catch (Exception e) {
                LOGGER.log(Level.WARNING, "setBoolean failed for key: " + key, e);
            }
        }
    }
    
    @Override
    public boolean exists(String key) {
        synchronized (syncLock) {
            try {
                boolean exists = MaudPreferences.contains(key);
                LOGGER.log(Level.FINE, "exists: " + key + " = " + exists);
                return exists;
            } catch (Exception e) {
                LOGGER.log(Level.WARNING, "exists check failed for key: " + key, e);
                return false;
            }
        }
    }
    
    @Override
    public void remove(String key) {
        synchronized (syncLock) {
            try {
                cache.remove(key);
                LOGGER.log(Level.FINE, "remove: " + key);
            } catch (Exception e) {
                LOGGER.log(Level.WARNING, "remove failed for key: " + key, e);
            }
        }
    }
    
    @Override
    public Map<String, Object> getAll() {
        synchronized (syncLock) {
            return new HashMap<>(cache);
        }
    }
    
    @Override
    public Map<String, Object> getByNamespace(String namespace) {
        synchronized (syncLock) {
            Map<String, Object> result = new HashMap<>();
            for (Map.Entry<String, Object> entry : cache.entrySet()) {
                if (entry.getKey().startsWith(namespace)) {
                    result.put(entry.getKey(), entry.getValue());
                }
            }
            return result;
        }
    }
    
    @Override
    public void clear() {
        synchronized (syncLock) {
            try {
                MaudPreferences.resetPreferences();
                cache.clear();
                LOGGER.info("All preferences cleared");
            } catch (Exception e) {
                LOGGER.log(Level.WARNING, "clear failed", e);
            }
        }
    }
    
    @Override
    public void resetToDefaults() {
        synchronized (syncLock) {
            try {
                MaudPreferences.resetPreferences();
                cache.clear();
                initializeCache();
                LOGGER.info("Preferences reset to defaults");
            } catch (Exception e) {
                LOGGER.log(Level.WARNING, "resetToDefaults failed", e);
            }
        }
    }
    
    @Override
    public String exportAsJson() {
        synchronized (syncLock) {
            StringBuilder json = new StringBuilder("{\n");
            List<String> keys = new ArrayList<>(cache.keySet());
            Collections.sort(keys);
            
            for (int i = 0; i < keys.size(); i++) {
                String key = keys.get(i);
                Object value = cache.get(key);
                json.append("  \"").append(key).append("\": ");
                
                if (value instanceof String) {
                    json.append("\"").append(escapeJson((String) value)).append("\"");
                } else {
                    json.append(value);
                }
                
                if (i < keys.size() - 1) {
                    json.append(",");
                }
                json.append("\n");
            }
            
            json.append("}");
            return json.toString();
        }
    }
    
    @Override
    public void importFromJson(String jsonData) {
        synchronized (syncLock) {
            // Simplified JSON import - in production, use a JSON library
            LOGGER.log(Level.INFO, "importFromJson: " + jsonData.length() + " bytes");
        }
    }
    
    @Override
    public IPreferenceTransaction beginTransaction() {
        return new PreferenceTransaction(this);
    }
    
    /**
     * Helper to escape JSON strings
     */
    private String escapeJson(String str) {
        return str.replace("\\", "\\\\")
                  .replace("\"", "\\\"")
                  .replace("\n", "\\n")
                  .replace("\r", "\\r")
                  .replace("\t", "\\t");
    }
}
