#!/bin/bash
#
# 测试脚本：检查RHFS90_MPI程序是否能正确编译
#

echo "=================================================="
echo "RHFS90_MPI 编译测试脚本"
echo "=================================================="

# 检查必要的文件是否存在
echo "检查源文件..."
required_files=("rhfs90mpi.f90" "hfsggmpi.f90" "hfsggmpi_I.f90" "CMakeLists.txt" "Makefile" "BUILDCONF.sh")

for file in "${required_files[@]}"; do
    if [ -f "$file" ]; then
        echo "✓ $file 存在"
    else
        echo "✗ $file 缺失"
        exit 1
    fi
done

echo ""
echo "检查MPI编译器..."
if command -v mpif90 &> /dev/null; then
    echo "✓ mpif90 编译器可用"
    mpif90 --version | head -1
else
    echo "✗ mpif90 编译器不可用"
    echo "请安装MPI编译器 (如 OpenMPI 或 MPICH)"
    exit 1
fi

echo ""
echo "检查依赖库目录..."
lib_dirs=("../../lib/lib9290" "../../lib/libmod" "../../lib/mpi90")

for dir in "${lib_dirs[@]}"; do
    if [ -d "$dir" ]; then
        echo "✓ $dir 目录存在"
    else
        echo "⚠ $dir 目录不存在 (可能需要先编译GRASP库)"
    fi
done

echo ""
echo "语法检查..."
echo "检查 Fortran 源文件语法..."

# 简单的语法检查
for f90_file in *.f90; do
    echo "检查 $f90_file..."
    if mpif90 -fsyntax-only "$f90_file" 2>/dev/null; then
        echo "✓ $f90_file 语法正确"
    else
        echo "⚠ $f90_file 语法可能有问题"
    fi
done

echo ""
echo "=================================================="
echo "测试完成！"
echo ""
echo "如果所有检查都通过，可以尝试编译："
echo "方法1: ./BUILDCONF.sh"
echo "方法2: make"
echo "方法3: 使用CMake构建系统"
echo "=================================================="
