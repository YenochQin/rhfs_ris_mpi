#!/bin/bash
#
# 测试脚本：验证rhfs90_mpi是否能通过CMake正确编译
#

echo "=================================================="
echo "RHFS90_MPI CMake构建测试脚本"
echo "=================================================="

# 检查是否在正确的目录
if [ ! -f "CMakeLists.txt" ]; then
    echo "❌ 错误：请在rhfs_ris_mpi根目录下运行此脚本"
    exit 1
fi

echo "✅ 当前目录：$(pwd)"
echo ""

# 检查MPI是否可用
echo "检查MPI环境..."
if command -v mpif90 &> /dev/null; then
    echo "✅ mpif90 编译器可用"
    mpif90 --version | head -1
else
    echo "⚠️  mpif90 编译器不可用，但CMake可能会找到其他MPI编译器"
fi

echo ""

# 检查关键文件是否存在
echo "检查关键文件..."
required_files=(
    "src/appl/rhfs90_mpi/rhfs90mpi.f90"
    "src/appl/rhfs90_mpi/hfsggmpi.f90" 
    "src/appl/rhfs90_mpi/hfsggmpi_I.f90"
    "src/appl/rhfs90_mpi/CMakeLists.txt"
    "src/lib/mpi90/CMakeLists.txt"
)

for file in "${required_files[@]}"; do
    if [ -f "$file" ]; then
        echo "✅ $file"
    else
        echo "❌ $file 缺失"
        exit 1
    fi
done

echo ""

# 创建构建目录
echo "创建构建目录..."
if [ -d "build" ]; then
    echo "⚠️  build目录已存在，将被清理"
    rm -rf build
fi
mkdir build
cd build

echo ""

# 运行CMake配置
echo "运行CMake配置..."
echo "执行命令：cmake .."
if cmake .. 2>&1 | tee cmake_output.log; then
    echo "✅ CMake配置成功"
    
    # 检查是否找到了MPI
    if grep -q "MPI_Fortran_FOUND" cmake_output.log; then
        echo "✅ 检测到MPI支持"
    else
        echo "⚠️  未检测到MPI支持，rhfs90mpi可能不会被构建"
    fi
    
    # 检查rhfs90mpi目标是否被包含
    if grep -q "rhfs90_mpi" cmake_output.log || [ -f "src/appl/rhfs90_mpi/Makefile" ]; then
        echo "✅ rhfs90mpi目标已配置"
    else
        echo "⚠️  rhfs90mpi目标可能未正确配置"
    fi
    
else
    echo "❌ CMake配置失败"
    echo "错误日志："
    cat cmake_output.log
    exit 1
fi

echo ""

# 尝试构建
echo "尝试构建rhfs90mpi..."
echo "执行命令：make rhfs90mpi"
if make rhfs90mpi 2>&1 | tee build_output.log; then
    echo "✅ rhfs90mpi编译成功！"
    
    # 检查二进制文件是否生成
    if [ -f "bin/rhfs90mpi" ]; then
        echo "✅ 二进制文件已生成：bin/rhfs90mpi"
        ls -la bin/rhfs90mpi
    else
        echo "⚠️  二进制文件未在预期位置找到"
    fi
    
else
    echo "❌ rhfs90mpi编译失败"
    echo "构建日志："
    tail -20 build_output.log
    exit 1
fi

echo ""
echo "=================================================="
echo "测试完成！"
echo ""
echo "如果编译成功，您可以使用以下命令运行程序："
echo "cd $(pwd)"
echo "mpirun -np <进程数> bin/rhfs90mpi"
echo ""
echo "要安装到项目bin目录，请运行："
echo "make install"
echo "=================================================="
