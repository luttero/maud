/*
 * AI Agent Refactoring Phase 1
 * Transaction Interface for Batch Preference Operations
 * 
 * Copyright (c) 2026 AI Enhancement
 */

package it.unitn.ing.rista.agent.api;

/**
 * IPreferenceTransaction: Transaction management for batch operations
 * 
 * Allows atomic batch updates to preferences with rollback support.
 * 
 * @author AI Agent Refactoring
 * @version 1.0
 */
public interface IPreferenceTransaction {
    
    /**
     * Add a string value to transaction
     */
    IPreferenceTransaction putString(String key, String value);
    
    /**
     * Add an integer value to transaction
     */
    IPreferenceTransaction putInt(String key, int value);
    
    /**
     * Add a double value to transaction
     */
    IPreferenceTransaction putDouble(String key, double value);
    
    /**
     * Add a long value to transaction
     */
    IPreferenceTransaction putLong(String key, long value);
    
    /**
     * Add a boolean value to transaction
     */
    IPreferenceTransaction putBoolean(String key, boolean value);
    
    /**
     * Remove a key from transaction
     */
    IPreferenceTransaction remove(String key);
    
    /**
     * Commit the transaction (atomic operation)
     * @return true if commit succeeded
     */
    boolean commit();
    
    /**
     * Rollback the transaction
     * @return true if rollback succeeded
     */
    boolean rollback();
    
    /**
     * Check if transaction is active
     */
    boolean isActive();
}
