# MAUD AI Agent Phase 2: Data File I/O Service

## Overview

Phase 2 provides a comprehensive, agent-friendly interface for data file operations.

### Components

#### 1. **IDataFileService** (Interface)
- Format auto-detection
- Multi-format support (MCA, CIF, DAT, XYE, RAW)
- File validation and metadata extraction
- Format conversion
- Data statistics calculation
- Large file streaming

#### 2. **FileFormatRegistry** (Format Management)
- Centralized format registration
- Format metadata storage
- Extension-based format lookup
- Extensible registry pattern

#### 3. **DataFileServiceImpl** (Implementation)
- Singleton pattern for centralized access
- Thread-safe file operations
- Comprehensive error handling
- Logging and monitoring
- Support for text and binary formats
- Streaming for large files

#### 4. **MCP Tools**
- `LoadDataFileTool`: Load data files with auto-detection
- `DataStatisticsTool`: Extract statistical information
- `FileFormatInfoTool`: Query supported formats

### Supported Formats

| Format | Extension | Type | Description |
|--------|-----------|------|-------------|
| MCA | .mca | Binary | Multi-Channel Analyzer (AMTEK standard) |
| CIF | .cif | Text | Crystallographic Information File |
| DAT | .dat | Text | Generic X-Y data format |
| XYE | .xye | Text | X-Y-Error format (3 columns) |
| RAW | .raw | Binary | Raw detector output |

### Usage Examples

#### Load a Data File
```java
DataFileServiceImpl fileService = DataFileServiceImpl.getInstance();

// Auto-detect format
Map<String, Object> data = fileService.loadDataFile("/path/to/data.mca");

// Specify format explicitly
data = fileService.loadDataFile("/path/to/data.txt", "xye");
```

#### Get File Statistics
```java
Map<String, Object> stats = fileService.getDataStatistics("/path/to/data.dat");
System.out.println("Min: " + stats.get("min"));
System.out.println("Max: " + stats.get("max"));
System.out.println("Mean: " + stats.get("mean"));
```

#### Validate File Before Loading
```java
IDataFileService.ValidationResult validation = fileService.validateFile("/path/to/file");
if (validation.isValid) {
    Map<String, Object> data = fileService.loadDataFile("/path/to/file");
}
```

#### Stream Large Files
```java
List<Map<String, Object>> chunks = fileService.streamDataInChunks("/path/to/large_file.dat", 1000);
for (Map<String, Object> chunk : chunks) {
    List<String> lines = (List<String>) chunk.get("lines");
    // Process chunk
}
```

#### Convert Between Formats
```java
booleanSuccess = fileService.convertFormat(
    "/path/to/input.mca",
    "/path/to/output.dat",
    "dat"
);
```

### MCP Integration

```java
// Register data file tools with MCP server
MCPServer mcpServer = new MCPServer("MAUD-Agent", "1.0");
mcpServer.registerTool(new LoadDataFileTool());
mcpServer.registerTool(new DataStatisticsTool());
mcpServer.registerTool(new FileFormatInfoTool());
mcpServer.start();

// Execute from agent
Map<String, Object> result = mcpServer.executeTool("load_data_file",
    Map.of("filePath", "/data/experiment.mca"));
```

### Future Enhancements

- Full MCA binary format parser
- CIF format parser with crystallographic data
- NetCDF support for scientific data
- HDF5 support for large datasets
- Compression support (gzip, zip)
- Database backend integration

---

*Phase 2 Complete - Data File I/O Service Ready*
