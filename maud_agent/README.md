# MAUD AI Agent Interface

将 MAUD (Materials Analysis Using Diffraction) 打造为 AI Agent 友好的格式。

## 架构

```
maud_agent/
├── README.md                  # 本文件
├── mcp_server.py              # MCP 服务器 (Model Context Protocol)
├── maud_wrapper.py            # Python 库，封装 MAUD 批处理接口
├── maud_pipeline.py           # 高级管线：自动完成 XRD 精修工作流
├── templates/                 # 模板指令文件
│   └── basic_refinement.ins   # 基本精修模板
├── examples/                  # 示例脚本
│   ├── run_refinement.py      # 运行 Rietveld 精修
│   ├── batch_loop.py          # 批量处理多个样品
│   └── export_results.py      # 导出各类结果
└── docs/
    ├── api_reference.md       # API 参考文档
    ├── mcp_tools.md           # MCP 工具说明
    └── batch_commands.md      # 批处理指令格式参考
```

## 快速开始

1. 确保已安装 MAUD jar 或可执行文件
2. 安装 Python 依赖: `pip install mcp`
3. 启动 MCP 服务器: `python maud_agent/mcp_server.py`

## MCP 服务器

MCP (Model Context Protocol) 服务器将 MAUD 的所有功能暴露为 AI Agent 可直接调用的工具。

支持的工具包括:
- `load_analysis` - 加载 .par 分析文件
- `run_refinement` - 运行 Rietveld 精修
- `run_compute` - 仅计算（不精修）
- `add_data_file` - 添加衍射数据文件
- `import_phase` - 导入相结构
- `save_analysis` - 保存分析结果
- `export_plot` - 导出拟合图
- `export_pole_figures` - 导出极图
- `export_diffraction_data` - 导出衍射数据 (CIF)
- `export_stress` - 导出应力数据
- `set_iterations` - 设置精修迭代次数
- `clear_data` - 清除数据文件
- `clear_phases` - 清除相
- `convert_format` - 数据格式转换
