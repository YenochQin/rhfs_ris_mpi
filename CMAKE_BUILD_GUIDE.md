# RHFS90_MPI CMake构建指南

## 概述

rhfs90_mpi已成功集成到GRASP项目的CMake构建系统中。您现在可以在`/Users/yiqin/Documents/ProjectFiles/rhfs_ris_mpi`目录下使用CMake来编译整个项目，包括rhfs90_mpi。

## 修改内容

### 1. 主CMakeLists.txt
在MPI应用程序部分添加了：
```cmake
add_subdirectory("src/appl/rhfs90_mpi")
```

### 2. rhfs90_mpi/CMakeLists.txt
已优化为符合GRASP项目标准，包括：
- 正确的库依赖 (mod, 9290, mpiu90)
- MPI编译标志设置
- 模块目录配置

## 编译方法

### 方法1：完整构建 (推荐)

```bash
cd /Users/yiqin/Documents/ProjectFiles/rhfs_ris_mpi
mkdir build
cd build
cmake ..
make
```

这将构建整个GRASP项目，包括所有库和应用程序。

### 方法2：仅构建rhfs90mpi

```bash
cd /Users/yiqin/Documents/ProjectFiles/rhfs_ris_mpi
mkdir build
cd build
cmake ..
make rhfs90mpi
```

### 方法3：使用测试脚本

```bash
cd /Users/yiqin/Documents/ProjectFiles/rhfs_ris_mpi
./test_cmake_build.sh
```

此脚本会自动检查环境并尝试构建rhfs90mpi。

## 依赖关系

rhfs90mpi依赖以下库，CMake会自动处理：
- `mod` - GRASP模块库
- `9290` - GRASP核心库  
- `mpiu90` - GRASP MPI工具库

## MPI检测

CMake会自动检测系统中的MPI：
- 如果找到MPI，将构建所有MPI程序，包括rhfs90mpi
- 如果未找到MPI，将跳过MPI程序的构建

## 编译标志

以下编译标志会自动应用：
- `-fno-automatic` (GRASP标准)
- MPI编译器标志
- 优化标志 (Release模式)

## 输出位置

编译完成后：
- 二进制文件：`build/bin/rhfs90mpi`
- 库文件：`build/lib/`

## 安装

要将程序安装到项目根目录的bin/文件夹：

```bash
make install
```

安装后可在 `/Users/yiqin/Documents/ProjectFiles/rhfs_ris_mpi/bin/rhfs90mpi` 找到程序。

## 运行程序

```bash
# 从build目录运行
cd build
mpirun -np <进程数> bin/rhfs90mpi

# 或安装后运行  
cd /Users/yiqin/Documents/ProjectFiles/rhfs_ris_mpi
mpirun -np <进程数> bin/rhfs90mpi
```

## 故障排除

### MPI未找到
如果CMake未检测到MPI：
1. 确保安装了MPI (OpenMPI, MPICH等)
2. 确保`mpif90`在PATH中
3. 可能需要设置MPI环境变量

### 编译错误
如果出现编译错误：
1. 检查所有源文件是否存在
2. 确保库依赖正确
3. 查看详细的编译日志

### 清理构建
要重新开始构建：
```bash
rm -rf build
mkdir build  
cd build
cmake ..
make
```

## 调试模式

要构建调试版本：
```bash
cmake -DCMAKE_BUILD_TYPE=Debug ..
make
```

## 并行构建

要加速构建过程：
```bash
make -j$(nproc)  # Linux
make -j$(sysctl -n hw.ncpu)  # macOS
```

## 验证构建

运行测试脚本验证一切正常：
```bash
./test_cmake_build.sh
```

这个脚本会检查环境、配置CMake、构建程序，并报告任何问题。
