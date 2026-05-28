# MAUD AI Agent API Layer

## Phase 1: Configuration Management

This module provides AI-agent-friendly interfaces and implementations for the MAUD application.

### Components

#### 1. **IPreferencesService** (Interface)
- Clean API for preference management
- Type-safe operations (String, int, double, long, boolean)
- Namespace support for grouped preferences
- JSON import/export
- Transaction support

#### 2. **ConfigManager** (Implementation)
- Singleton pattern for centralized access
- Thread-safe operations with synchronized cache
- Wraps MaudPreferences with enhanced functionality
- Comprehensive logging
- Memory caching for performance

#### 3. **PreferenceTransaction** (Implementation)
- Atomic batch operations
- Rollback support
- Fluent API for chaining operations

#### 4. **MCPServer** (MCP Foundation)
- Model Context Protocol server base
- Tool registration and execution
- Foundation for agent communication

#### 5. **ToolDefinition** (Abstract)
- Base class for defining MCP tools
- Parameter specification
- Tool metadata

#### 6. **PreferencesTool** (Tool Implementation)
- First MCP tool for preference management
- Extensible for additional operations

### Usage Example

```java
// Get ConfigManager instance
ConfigManager config = ConfigManager.getInstance();

// Read preferences
String uiTheme = config.getString("ui.theme", "default");
int maxThreads = config.getInt("computation.threads", 4);

// Write preferences
config.setString("ui.theme", "dark");
config.setInt("computation.threads", 8);

// Batch operations with transaction
IPreferenceTransaction txn = config.beginTransaction();
txn.putString("key1", "value1")
   .putInt("key2", 42)
   .putBoolean("key3", true)
   .commit();

// Export/Import
String json = config.exportAsJson();
config.importFromJson(json);
```

### MCP Integration

```java
// Initialize MCP Server
MCPServer mcpServer = new MCPServer("MAUD-Agent", "1.0");

// Register tools
mcpServer.registerTool(new PreferencesTool());

// Start server
mcpServer.start();

// Execute tool from agent
Map<String, Object> result = mcpServer.executeTool("get_preference", 
    Map.of("key", "ui.theme"));
```

### Future Phases

- Phase 2: Data File I/O Service
- Phase 3: Diffraction Analysis API
- Phase 4: Computation Service Layer
- Phase 5: REST API Gateway

---

*AI Agent Refactoring Initiative - Making MAUD Agent-Friendly*
