#!/bin/bash

# GRASP MPI版本的CMake构建脚本
# 使用方法: ./build_cmake.sh [clean]

echo "GRASP MPI版本 CMake构建系统"
echo "============================="

# 获取当前目录
PROJECT_DIR=$(pwd)

# 如果指定了clean参数，删除构建目录
if [[ "$1" == "clean" ]]; then
    echo "清理构建目录..."
    rm -rf build/
    exit 0
fi

# 创建构建目录
if [ ! -d "build" ]; then
    echo "创建构建目录..."
    mkdir build
fi

cd build

echo "运行CMake配置..."
# 配置项目，要求找到MPI
cmake .. -DCMAKE_BUILD_TYPE=Release

if [ $? -ne 0 ]; then
    echo "❌ CMake配置失败！"
    echo "请确保已安装MPI开发环境:"
    echo "  macOS: brew install open-mpi"
    echo "  Ubuntu/Debian: sudo apt install libopenmpi-dev"
    echo "  CentOS/RHEL: sudo yum install openmpi-devel"
    exit 1
fi

echo "编译项目..."
make -j$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)

if [ $? -eq 0 ]; then
    echo "✅ 构建成功！"
    echo ""
    echo "生成的可执行文件位于:"
    echo "  $(pwd)/bin/"
    echo ""
    echo "可用的程序:"
    if [ -f "bin/rhfs_mpi" ]; then
        echo "  - rhfs_mpi (RHFS MPI version)"
    fi
    if [ -f "bin/rhfszeeman95_mpi" ]; then
        echo "  - rhfszeeman95_mpi (RHFS Zeeman MPI version)"
    fi
    if [ -f "bin/ris4_mpi" ]; then
        echo "  - ris4_mpi (RIS4 MPI version)"
    fi
    echo ""
    echo "安装到bin/目录: make install"
else
    echo "❌ 构建失败！请检查错误信息。"
    exit 1
fi 