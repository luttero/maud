# MAUD MCP 工具参考

MAUD (Materials Analysis Using Diffraction) 的 MCP 服务器暴露了以下工具，
供任何兼容 MCP 协议的 AI 代理（如 Claude Desktop、Cursor、VS Code 等）使用。

## 启动 MCP 服务器

### 方式一：直接运行
```bash
python maud_agent/mcp_server.py
```

### 方式二：通过 MCP 主机配置
在 Claude Desktop 或 Cursor 的 MCP 配置中添加：

```json
{
  "mcpServers": {
    "maud-xrd": {
      "command": "python",
      "args": ["/path/to/maud/maud_agent/mcp_server.py"],
      "env": {
        "MAUD_HOME": "/path/to/maud",
        "JAVA_HOME": "/path/to/jdk"
      }
    }
  }
}
```

## 工具清单

### 1. run_refinement (核心工具)
运行 Rietveld 精修。

参数:
- `analysis_file` (string, 必填) - .par 分析文件路径
- `iterations` (int, 默认 10) - 精修迭代次数
- `output_file` (string, 可选) - 输出 .par 文件路径
- `data_file` (string, 可选) - 添加衍射数据文件
- `phase_file` (string, 可选) - 导入相结构
- `wizard_mode` (int, 默认 0) - 精修向导模式 (0=标准, -1=仅计算)
- `workdir` (string, 可选) - 工作目录
- `timeout` (int, 默认 600) - 超时秒数

返回:
- `status`, `rw`, `rexp`, `gof`, `output_summary`

### 2. load_analysis
加载 .par 分析文件（不执行）。

### 3. run_compute
仅计算衍射图谱，不优化参数。适用于模型检查。

### 4. add_data_file
向分析中添加衍射数据文件。

支持的格式: .xy, .dat, .raw, .PRN, .F1B, .ddq, .tif, .jpg, .hdf

### 5. import_phase
导入晶体结构相。

支持的格式: .cif, .par, .apf

### 6. export_plot
导出拟合图 PNG（每条数据一个图）。

### 7. export_pole_figures
导出极图（织构分析）。

参数 `phases` 格式: `"0 1 1 1 2 0 0 2 2 0"`
表示：第0相, hkl=(1,1,1), (2,0,0), (2,2,0)

### 8. export_diff_data
导出实验和计算衍射数据为 CIF 文件。

### 9. export_stress
导出残余应力分析结果（sin²ψ 方法）。

### 10. batch_workflow
执行多步精修工作流，通过 JSON 定义步骤序列。

示例:
```json
[
  {"action": "load", "file": "sample.par", "out": "s1.par"},
  {"action": "add_data", "file": "s1.par", "data": "data.xy", "out": "s2.par"},
  {"action": "refine", "file": "s2.par", "iterations": 20, "out": "final.par"},
  {"action": "plot", "file": "final.par", "png": "fit_plot"},
  {"action": "export_diff", "file": "final.par", "output": "results.cif"}
]
```

### 11. generate_ins_script
生成 .ins 指令文件但不执行。

## Python 库 API

直接使用 Python API（比 MCP 更灵活）：

```python
from maud_agent.maud_wrapper import MaudBatch
from maud_agent.maud_pipeline import MaudPipeline

# 低级 API
m = MaudBatch(maud_home="/path/to/maud")
m.load_analysis("sample.par")
m.set_iterations(20)
m.add_data_file("data.xy")
m.export_plot("output")
result = m.run()

# 高级管线
p = MaudPipeline(maud_home="/path/to/maud")
p.full_refinement(
    par_file="sample.par",
    data_file="data.xy",
    iterations=20,
    export_plot="fit.png",
)
```
