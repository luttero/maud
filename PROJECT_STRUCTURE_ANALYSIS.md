# MAUD Project Structure & Code Analysis

## 📊 Project Overview

**MAUD** (Materials Analysis Using Diffraction)  
**Repository**: RietveldXRD/maud (forked from luttero/maud)  
**Language**: Java (99.5%)  
**License**: BSD 3-Clause  
**Default Branch**: version2  
**Repository Size**: ~330 MB

---

## 🏗️ Directory Structure

```
maud/
├── src/                          # Main source code directory
│   ├── Maud/                     # Maud core module
│   ├── HTTPClient/               # HTTP client utilities
│   ├── Jama/                     # JAMA matrix library
│   ├── META-INF/                 # Metadata
│   ├── com/
│   │   └── radiographema/        # Core MAUD application classes
│   │       ├── Maud.java         # Main launcher
│   │       ├── MaudText.java     # Text-only mode launcher
│   │       └── MaudWebStart.java # WebStart launcher
│   ├── org/
│   │   ├── javadev/              # Java UI utilities (AnimatingCardLayout)
│   │   │   ├── effects/          # UI effects
│   │   │   └── test/             # Tests
│   │   ├── json/                 # JSON processing
│   │   └── la4j/                 # Linear algebra library
│   ├── it/
│   │   └── unitn/
│   │       └── ing/
│   │           ├── rista/        # Core RISTA package
│   │           │   ├── util/     # Utilities & preferences
│   │           │   ├── awt/      # GUI components
│   │           │   ├── diffr/    # Diffraction data handling
│   │           │   ├── comp/     # Computational methods
│   │           │   └── interfaces/ # Interface definitions
│   │           └── esqui/        # ESQUI integration (FTP server)
│   ├── gov/                      # Government/official packages
│   ├── ij/                       # ImageJ integration
│   │   └── gui/                  # Custom GUI dialogs
│   ├── jnt/                      # Numerical analysis
│   ├── net/                      # Network utilities
│   ├── files/                    # File I/O utilities
│   ├── examples/                 # Example data/files
│   ├── help/                     # Help documentation
│   ├── images/                   # Image resources
│   ├── license/                  # License files
│   ├── fr/                       # French localization
│   ├── it/                       # Italian localization
│   ├── gl4java/                  # OpenGL Java bindings
│   └── Todo.txt                  # Development todo list
├── docs/                         # Documentation
├── libs/                         # External libraries
├── media/                        # Media resources
├── ImageJ/                       # ImageJ plugin files
├── build.xml                     # Ant build configuration (194 KB)
├── ant_maud_v2.properties       # Build properties
├── Maud.iml                     # IntelliJ IDEA module file
├── Maud.ipr                     # IntelliJ IDEA project file
├── Maud.iws                     # IntelliJ IDEA workspace
├── Compile.md                   # Compilation guide
├── README.md                    # Project README
└── LICENSE                      # License file

```

---

## 🔍 Key Source Files Analysis

### Core Application Entry Points

| File | Purpose | Lines | Notes |
|------|---------|-------|-------|
| `src/com/radiographema/Maud.java` | Main GUI launcher | ~200 | Initializes Swing UI, preferences, main frame |
| `src/com/radiographema/MaudText.java` | Text-only launcher | ~100 | Batch processing, JPVM, XGrid support |
| `src/com/radiographema/MaudWebStart.java` | WebStart launcher | ~69 | JNLP support (mostly commented out) |

### UI Components

| Directory | Purpose | Key Files |
|-----------|---------|-----------|
| `org/javadev/` | UI utilities | AnimatingCardLayout.java (card transition effects) |
| `ij/gui/` | ImageJ GUI integration | MaudGenericDialog.java (custom dialog) |
| `it/unitn/ing/rista/awt/` | Custom AWT components | MaudetteMacOSFrame.java (macOS integration) |

### Data Processing

| File | Purpose |
|------|---------|
| `it/unitn/ing/rista/diffr/data/MCADatafile.java` | Multi-Channel Analyzer data format support |
| `it/unitn/ing/esqui/server/FtpServer.java` | FTP data transfer server |

### Computation & Algorithms

| File | Purpose | Notes |
|------|---------|-------|
| `it/unitn/ing/rista/comp/NelderMeadSimplex.java` | Optimization algorithm | Nelder-Mead simplex minimization |
| `org/la4j/` | Linear algebra | Matrix operations |
| `Jama/` | JAMA matrix library | Additional numerical computations |

### Utilities

| File | Purpose |
|------|---------|
| `it/unitn/ing/rista/util/MaudPreferences.java` | Preference/settings management |
| `it/unitn/ing/rista/util/` | General utilities (Misc, Constants, etc.) |
| `it/unitn/ing/rista/interfaces/` | Interface definitions for pluggable components |

---

## 📈 Estimated Code Statistics

Based on project analysis:

- **Total Repository Size**: ~330 MB
- **Estimated Lines of Code**: 50,000 - 150,000 lines (Java)
  - Core MAUD: ~30,000-50,000 LOC
  - Libraries (Jama, la4j, etc.): ~20,000-50,000 LOC
  - Dependencies & resources: ~50,000+ LOC
- **Build System**: Apache Ant (build.xml is 194 KB - very complex)
- **Number of Packages**: 20+ main packages
- **Main Modules**: 5-7 major functional areas

---

## 🏢 Major Functional Areas

### 1. **Core Application** (`com.radiographema`)
- Application entry points and initialization
- Swing UI setup
- Multi-mode execution (GUI, Text, WebStart)

### 2. **Diffraction Analysis** (`it.unitn.ing.rista`)
- **diffr**: Diffraction data file handling and analysis
- **comp**: Computational algorithms (Rietveld refinement, optimization)
- **util**: Utilities and preferences
- **interfaces**: Plugin system interfaces

### 3. **User Interface** (`ij.gui`, `it.unitn.ing.rista.awt`, `org.javadev`)
- ImageJ integration
- Custom Swing components
- macOS specific features
- Card-based layout animations

### 4. **Data I/O** (`it.unitn.ing.esqui`)
- FTP server for data transfer
- Multiple file format support (.mca, CIF, etc.)
- Data persistence

### 5. **Numerical Libraries** (`Jama`, `org.la4j`, `org.javadev`)
- Matrix operations
- Linear algebra
- Optimization algorithms

### 6. **Integration Features**
- ImageJ plugin support
- macOS native features
- OpenGL visualization (gl4java)
- HTTP client support

---

## 🔌 Integration Points for AI Agent Adaptation

### High Priority (Core to rewrite):
1. **Data Pipeline Classes** - Input/output handling
2. **Analysis Algorithms** - Diffraction computation
3. **Configuration Management** - Preferences system
4. **API Layer** - Create REST/gRPC endpoints

### Medium Priority (Refactor):
1. **UI Components** - Replace with API-driven architecture
2. **File I/O** - Abstract into service layer
3. **Preferences** - Convert to config management

### Lower Priority (Library integration):
1. Keep numerical libraries as-is
2. Maintain external dependencies
3. Wrap in service interfaces

---

## 🛠️ Build Information

- **Build Tool**: Apache Ant
- **Build File**: `build.xml` (194 KB - contains extensive compilation rules)
- **Properties File**: `ant_maud_v2.properties`
- **Java Version**: JDK 1.1+ support (legacy code, needs modernization)
- **IDE Support**: IntelliJ IDEA (.iml, .ipr, .iws files)

---

## 📝 Development Artifacts

- **TODO List**: `src/Todo.txt`
- **Compilation Guide**: `Compile.md`
- **Documentation**: `docs/` directory
- **Examples**: `src/examples/`

---

## 🎯 Recommended AI Agent Refactoring Roadmap

1. **Phase 1: API Foundation**
   - Create REST API layer wrapping core algorithms
   - Define service interfaces
   - Set up MCP (Model Context Protocol) endpoints

2. **Phase 2: Data Model Abstraction**
   - Refactor data structures for serialization
   - Create JSON/Protocol Buffer schemas
   - Abstract file I/O into services

3. **Phase 3: Algorithm Extraction**
   - Expose computational functions as callable services
   - Create parameter validation layer
   - Add monitoring and logging

4. **Phase 4: Agent Integration**
   - Implement Agent-friendly interfaces (Hermes/OpenClaw)
   - Create tool calling interface
   - Add context preservation mechanisms

5. **Phase 5: Testing & Optimization**
   - Unit test coverage
   - Performance profiling
   - API documentation generation

---

## 🔗 Key Dependencies & Libraries

- **JAMA**: Java matrix library (local copy)
- **la4j**: Linear algebra (local copy)
- **ImageJ**: Image processing integration
- **HTTP Client**: Network utilities
- **Swing**: GUI framework
- **JSON**: Data serialization (org.json)
- **OpenGL**: gl4java for visualization

---

## 📋 Next Steps

1. Extract specific file lists for each functional module
2. Analyze class hierarchies and dependencies
3. Create MCP service definitions
4. Design REST API schema
5. Plan incremental refactoring with commits

