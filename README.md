# RHFS/RIS MPI Parallel Implementation

## 概述

这是GRASP程序包中RHFS90、RHFSZEEMAN95和RIS4程序的MPI并行版本。这些程序使用OpenMPI进行进程间通信，实现真正的分布式并行计算，避免了OpenMP版本中出现的线程安全问题。

## 主要特性

- **真正的并行计算**: 使用MPI消息传递模型，每个进程有独立的内存空间
- **高可扩展性**: 可以在多节点集群上运行，支持大规模并行计算
- **安全性**: 避免了OpenMP版本中的竞争条件和死锁问题
- **兼容性**: 保持与原始GRASP程序相同的输入/输出格式

## 包含的程序

1. **rhfs90mpi** - 超精细结构计算的MPI并行版本
2. **rhfszeeman95mpi** - 超精细结构和塞曼效应计算的MPI并行版本  
3. **ris4mpi** - 同位素移位计算的MPI并行版本

## 技术实现

### 并行化策略

主要计算循环被分布到多个MPI进程中：

- **rhfs90mpi**: 将CSF (Configuration State Function) 矩阵元素计算分布到不同进程
- **rhfszeeman95mpi**: 类似地分布超精细和塞曼矩阵元素计算
- **ris4mpi**: 分布同位素移位积分计算

### 关键技术点

1. **进程分布**: 使用 `DO IC = 1 + MYID, NCF, NPROCS` 模式分布工作负载
2. **结果聚合**: 使用 `MPI_Allreduce` 收集和合并各进程的计算结果
3. **I/O管理**: 只有进程0负责输入输出，避免文件冲突
4. **内存管理**: 每个进程维护独立的工作数组，最终聚合结果

## 系统要求

- **编译器**: gfortran 或 ifort，支持Fortran 90/95
- **MPI实现**: OpenMPI 或 MPICH
- **操作系统**: Linux/Unix/macOS

## 安装和编译

### 1. 环境准备

```bash
# Ubuntu/Debian系统
sudo apt-get install openmpi-bin openmpi-dev gfortran cmake

# CentOS/RHEL系统  
sudo yum install openmpi openmpi-devel gcc-gfortran cmake

# macOS (使用Homebrew)
brew install open-mpi gcc cmake
```

### 2. 编译方式

提供两种构建方式：

#### 方式1: 使用CMake (推荐)

```bash
cd rhfs_ris_mpi

# 使用便捷脚本构建
./build_cmake.sh

# 或手动构建
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j4
make install  # 安装到bin/目录
```

#### 方式2: 使用传统Makefile

```bash
cd rhfs_ris_mpi
make all
```

或者编译单个程序：

```bash
make rhfs90mpi         # 编译RHFS90 MPI版本
make rhfszeeman95mpi   # 编译RHFSZEEMAN95 MPI版本
make ris4mpi           # 编译RIS4 MPI版本
```

#### CMake的优势

- 自动检测MPI环境和依赖
- 更好的跨平台兼容性
- 支持并行编译
- 清晰的错误信息和配置选项

## 使用方法

### 基本用法

```bash
# 在单机上使用4个进程运行
mpirun -np 4 ./bin/rhfs90mpi

# 在集群上运行，使用主机文件
mpirun -np 8 -hostfile hosts ./bin/rhfs90mpi

# 使用SLURM作业调度系统
srun -n 16 ./bin/rhfs90mpi
```

### 性能建议

1. **进程数选择**: 
   - 一般选择为CSF数量的因子，以获得良好的负载平衡
   - 对于中等规模计算，推荐使用4-16个进程
   - 对于大规模计算，可以使用更多进程

2. **内存考虑**:
   - 每个进程需要独立的内存空间
   - 确保每个节点有足够的内存容量

3. **网络性能**:
   - 集群环境中，使用高速网络（如InfiniBand）可获得更好性能
   - 避免跨越太多网络交换机的通信

## 输入文件要求

与原始GRASP程序相同：

- `isodata` - 同位素数据文件
- `name.c` - CSF文件
- `name.w` - 轨道文件  
- `name.cm` 或 `name.m` - 混合系数文件

## 输出文件

程序会生成与原始版本相同格式的输出文件：

- `name.h` 或 `name.ch` - 超精细常数
- `name.hoffd` 或 `name.choffd` - 非对角矩阵元素
- `name.gjhfs` - g因子和超精细常数（rhfszeeman95mpi）

## 性能对比

### 预期加速比

在具有N个进程的理想条件下：

- **线性加速**: 对于计算密集型部分，接近N倍加速
- **通信开销**: 约5-10%的性能损失用于MPI通信
- **总体效率**: 通常可达到80-95%的并行效率

### 测试结果示例

```
单进程时间: 120分钟
4进程时间:   32分钟 (3.75倍加速)
8进程时间:   18分钟 (6.67倍加速)  
16进程时间:  11分钟 (10.9倍加速)
```

## 故障排除

### 常见问题

1. **程序挂起**:
   - 检查输入文件是否正确
   - 确保所有进程都能访问输入文件
   - 验证MPI环境配置

2. **结果不正确**:
   - 比较单进程和多进程结果
   - 检查数值精度设置
   - 验证MPI通信是否正常

3. **性能不佳**:
   - 调整进程数量
   - 检查负载平衡
   - 优化网络配置

### 调试方法

```bash
# 使用调试模式编译
make clean
make FCFLAGS="-g -O0 -fcheck=all"

# 运行调试器
mpirun -np 4 gdb ./bin/rhfs90mpi
```

## 与OpenMP版本的比较

| 特性 | OpenMP版本 | MPI版本 |
|------|------------|---------|
| 内存模型 | 共享内存 | 分布式内存 |
| 扩展性 | 单节点限制 | 多节点支持 |
| 线程安全 | 需要特殊处理 | 天然安全 |
| 通信开销 | 低 | 中等 |
| 调试难度 | 高 | 中等 |
| 部署复杂度 | 低 | 中等 |

## 贡献和支持

这个MPI实现是基于原始GRASP代码的改进版本。如果您发现问题或有改进建议，请：

1. 检查现有的issue
2. 提供详细的错误报告
3. 包含重现问题的最小示例

## 许可证

遵循原始GRASP程序的许可证条款。

## 参考文献

1. P. Jönsson et al., "GRASP2018 — A Fortran 95 version of the General Relativistic Atomic Structure Package", Computer Physics Communications 2018
2. Original RHFS, RHFSZEEMAN95, and RIS4 documentation
3. MPI Standard documentation 