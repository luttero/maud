/*
 * AI Agent Refactoring Phase 1
 * Transaction Implementation for Batch Operations
 * 
 * Copyright (c) 2026 AI Enhancement
 */

package it.unitn.ing.rista.agent.config;

import it.unitn.ing.rista.agent.api.IPreferenceTransaction;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * PreferenceTransaction: Atomic transaction support for batch preference updates
 * 
 * @author AI Agent Refactoring
 * @version 1.0
 */
public class PreferenceTransaction implements IPreferenceTransaction {
    
    private static final Logger LOGGER = Logger.getLogger(PreferenceTransaction.class.getName());
    private final ConfigManager configManager;
    private final Map<String, Object> transactionData;
    private final Map<String, Object> originalData;
    private boolean isActive;
    
    public PreferenceTransaction(ConfigManager configManager) {
        this.configManager = configManager;
        this.transactionData = new HashMap<>();
        this.originalData = new HashMap<>(configManager.getAll());
        this.isActive = true;
    }
    
    @Override
    public IPreferenceTransaction putString(String key, String value) {
        if (!isActive) throw new IllegalStateException("Transaction is not active");
        transactionData.put(key, value);
        return this;
    }
    
    @Override
    public IPreferenceTransaction putInt(String key, int value) {
        if (!isActive) throw new IllegalStateException("Transaction is not active");
        transactionData.put(key, value);
        return this;
    }
    
    @Override
    public IPreferenceTransaction putDouble(String key, double value) {
        if (!isActive) throw new IllegalStateException("Transaction is not active");
        transactionData.put(key, value);
        return this;
    }
    
    @Override
    public IPreferenceTransaction putLong(String key, long value) {
        if (!isActive) throw new IllegalStateException("Transaction is not active");
        transactionData.put(key, value);
        return this;
    }
    
    @Override
    public IPreferenceTransaction putBoolean(String key, boolean value) {
        if (!isActive) throw new IllegalStateException("Transaction is not active");
        transactionData.put(key, value);
        return this;
    }
    
    @Override
    public IPreferenceTransaction remove(String key) {
        if (!isActive) throw new IllegalStateException("Transaction is not active");
        transactionData.put(key, null);
        return this;
    }
    
    @Override
    public boolean commit() {
        if (!isActive) return false;
        
        try {
            for (Map.Entry<String, Object> entry : transactionData.entrySet()) {
                if (entry.getValue() == null) {
                    configManager.remove(entry.getKey());
                } else if (entry.getValue() instanceof String) {
                    configManager.setString(entry.getKey(), (String) entry.getValue());
                } else if (entry.getValue() instanceof Integer) {
                    configManager.setInt(entry.getKey(), (Integer) entry.getValue());
                } else if (entry.getValue() instanceof Double) {
                    configManager.setDouble(entry.getKey(), (Double) entry.getValue());
                } else if (entry.getValue() instanceof Long) {
                    configManager.setLong(entry.getKey(), (Long) entry.getValue());
                } else if (entry.getValue() instanceof Boolean) {
                    configManager.setBoolean(entry.getKey(), (Boolean) entry.getValue());
                }
            }
            isActive = false;
            LOGGER.info("Transaction committed with " + transactionData.size() + " changes");
            return true;
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Transaction commit failed", e);
            return false;
        }
    }
    
    @Override
    public boolean rollback() {
        if (!isActive) return false;
        
        try {
            transactionData.clear();
            isActive = false;
            LOGGER.info("Transaction rolled back");
            return true;
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Transaction rollback failed", e);
            return false;
        }
    }
    
    @Override
    public boolean isActive() {
        return isActive;
    }
}
