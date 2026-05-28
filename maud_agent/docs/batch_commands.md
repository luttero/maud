# MAUD 批处理指令格式参考

MAUD 的批处理模式通过 `.ins` 文件（CIF 格式）控制。本文档列出所有可用指令。

## 基本结构

```
data_MAUD_batch
_riet_analysis_file "my_analysis.par"
_riet_analysis_iteration_number 10
loop_
_riet_meas_datafile_name
_riet_analysis_fileToSave
"file1.xy"
"output.par"
```

## 指令列表

### 分析控制

| 指令 | 用途 | 值类型 |
|------|------|--------|
| `_riet_analysis_file` | 加载 .par 分析文件 | string (文件路径) |
| `_riet_analysis_iteration_number` | 设置迭代次数 | int |
| `_riet_analysis_wizard_index` | 设置精修模式 | int (-1=仅计算, 0=精修, 1-99=向导) |
| `_riet_analysis_fileToSave` | 保存结果到文件 | string (文件路径) |

### 数据文件操作

| 指令 | 用途 | 值类型 |
|------|------|--------|
| `_riet_meas_datafile_name` | 添加衍射数据文件 | string (文件路径) |
| `_riet_meas_datafile_replace` | 替换已有数据 | true/false |
| `_maud_remove_all_datafiles` | 清除所有数据 | true |
| `_maud_working_directory` | 设置工作目录 | string (目录路径) |
| `_pd_meas_dataset_number` | 选择数据集编号 | int (0-based) |

### 相/结构操作

| 指令 | 用途 | 值类型 |
|------|------|--------|
| `_maud_remove_all_phases` | 清除所有相 | true |
| `_maud_import_phase` | 导入相(.cif/.par) | string (文件路径) |

### 背景处理

| 指令 | 用途 | 值类型 |
|------|------|--------|
| `_maud_background_add_automatic` | 自动多项式背景 | true/false |

### 导出结果

| 指令 | 用途 | 值类型 |
|------|------|--------|
| `_maud_output_plot_filename` | 导出拟合 PNG 图 | string (基础名) |
| `_maud_output_plot2D_filename` | 导出 2D 衍射图 | string (基础名) |
| `_maud_export_pole_figures_filename` | 极图输出文件名 | string (文件路径) |
| `_maud_export_pole_figures_options` | 极图选项 | string (格式见下) |
| `_maud_export_pole_figures` | 极图反射列表 | string (格式见下) |
| `_maud_output_stress_filename` | 应力输出文件名 | string (文件路径) |
| `_maud_output_stress_options` | 应力选项 | string (格式见下) |
| `_maud_output_diff_data_filename` | 导出衍射数据(CIF) | string (文件路径) |
| `_riet_append_simple_result_to` | 追加简单结果 | string (文件路径) |
| `_riet_append_result_to` | 追加完整结果 | string (文件路径) |

### 仪器设置

| 指令 | 用途 | 值类型 |
|------|------|--------|
| `_riet_meas_datains_name` | 从脚本加载仪器 | string (文件路径) |

### LCLS2 专用

| 指令 | 用途 |
|------|------|
| `_maud_LCLS2_Cspad0_original_image` | 原始图像路径 |
| `_maud_LCLS2_Cspad0_dark_image` | 暗场图像路径 |
| `_maud_LCLS2_detector_config_file` | 探测器配置文件 |

### 其他

| 指令 | 用途 | 值类型 |
|------|------|--------|
| `_publ_section_title` | 分析标题 | string |
| `_maud_export_lumaCAM_to_GSAS_datafile` | 导出为 GSAS 格式 | string (文件路径) |

## 极图/应力选项格式

极图反射列表和应力选项使用相同的格式：

```
P0 1 1 1 2 0 0 2 2 0 P1 1 1 1 2 0 0
```

说明：
- `P0`, `P1` = 相编号
- 后面的数字是 h k l 三元组
- `P0 1 1 1 2 0 0` = 第0相，反射 (1,1,1) 和 (2,0,0)

## 完整示例

### 基本精修

```
data_MAUD_batch
_riet_analysis_file "sample.par"
_riet_analysis_iteration_number 20
_riet_analysis_fileToSave "sample_refined.par"
_riet_meas_datafile_name "data.xy"
_maud_export_pole_figures "P0 1 1 1 2 0 0"
_maud_output_plot_filename "fit"
_maud_output_diff_data_filename "diffraction.cif"
```

### 仅计算（不精修）

```
data_MAUD_batch
_riet_analysis_file "sample.par"
_riet_analysis_wizard_index -1
_riet_analysis_fileToSave "computed.par"
```

### 多数据集循环

```
data_MAUD_batch
loop_
_riet_analysis_file
_riet_analysis_iteration_number
_riet_analysis_fileToSave
"sample1.par" 5 "sample1_refined.par"
"sample2.par" 5 "sample2_refined.par"
"sample3.par" 5 "sample3_refined.par"
```
