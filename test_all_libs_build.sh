#!/bin/bash
#
# 完整构建测试脚本 - 测试所有库和rhfs90_mpi的构建
#

echo "=================================================="
echo "RHFS90_MPI 完整构建测试 (包含所有库)"
echo "=================================================="

# 检查是否在正确的目录
if [ ! -f "CMakeLists.txt" ]; then
    echo "❌ 错误：请在rhfs_ris_mpi根目录下运行此脚本"
    exit 1
fi

echo "✅ 当前目录：$(pwd)"
echo ""

# 检查MPI
echo "检查MPI环境..."
if command -v mpif90 &> /dev/null; then
    echo "✅ mpif90 编译器可用"
    mpif90 --version | head -1
else
    echo "❌ mpif90 编译器不可用，rhfs90_mpi需要MPI支持"
    exit 1
fi

echo ""

# 检查必要文件
echo "检查所有库的CMakeLists.txt文件..."
required_libs=(
    "src/lib/libmod/CMakeLists.txt"
    "src/lib/lib9290/CMakeLists.txt"
    "src/lib/libdvd90/CMakeLists.txt"
    "src/lib/libmcp90/CMakeLists.txt"
    "src/lib/librang90/CMakeLists.txt"
    "src/lib/mpi90/CMakeLists.txt"
    "src/appl/rhfs90_mpi/CMakeLists.txt"
)

for file in "${required_libs[@]}"; do
    if [ -f "$file" ]; then
        echo "✅ $file"
    else
        echo "❌ $file 缺失"
        exit 1
    fi
done

echo ""

# 检查源文件
echo "检查rhfs90_mpi源文件..."
rhfs_sources=(
    "src/appl/rhfs90_mpi/rhfs90mpi.f90"
    "src/appl/rhfs90_mpi/hfsggmpi.f90"
    "src/appl/rhfs90_mpi/hfsggmpi_I.f90"
)

for file in "${rhfs_sources[@]}"; do
    if [ -f "$file" ]; then
        echo "✅ $file"
    else
        echo "❌ $file 缺失"
        exit 1
    fi
done

echo ""

# 清理并创建构建目录
echo "创建构建目录..."
if [ -d "build" ]; then
    echo "清理旧的build目录..."
    rm -rf build
fi
mkdir build
cd build

echo ""

# CMake配置
echo "运行CMake配置..."
if cmake .. 2>&1 | tee cmake_output.log; then
    echo "✅ CMake配置成功"
    
    # 检查MPI检测
    if grep -q "MPI support found for rhfs90_mpi" cmake_output.log; then
        echo "✅ MPI支持已正确检测"
    else
        echo "❌ MPI支持检测失败"
        exit 1
    fi
    
else
    echo "❌ CMake配置失败"
    cat cmake_output.log
    exit 1
fi

echo ""

# 构建所有GRASP库
echo "构建所有GRASP库..."

# 库名对应的make目标
libraries=("mod" "9290" "dvd90" "mcp90" "rang90" "mpiu90")
lib_names=("libmod" "lib9290" "libdvd90" "libmcp90" "librang90" "mpi90")

for i in "${!libraries[@]}"; do
    lib_target="${libraries[$i]}"
    lib_name="${lib_names[$i]}"
    echo "构建 $lib_name (目标: $lib_target)..."
    
    if make $lib_target 2>&1 | tee -a build_output.log; then
        echo "✅ $lib_name 构建成功"
    else
        echo "❌ $lib_name 构建失败"
        echo "最后15行日志："
        tail -15 build_output.log
        exit 1
    fi
done

echo ""

# 构建rhfs90mpi
echo "构建rhfs90mpi..."
if make rhfs90mpi 2>&1 | tee -a build_output.log; then
    echo "✅ rhfs90mpi构建成功！"
    
    if [ -f "bin/rhfs90mpi" ]; then
        echo "✅ 二进制文件已生成：bin/rhfs90mpi"
        echo "文件信息："
        ls -la bin/rhfs90mpi
    else
        echo "⚠️  二进制文件未在预期位置找到"
    fi
    
else
    echo "❌ rhfs90mpi构建失败"
    echo "最后20行构建日志："
    tail -20 build_output.log
    exit 1
fi

echo ""

# 验证构建结果
echo "验证构建结果..."
echo "生成的库文件："
if [ -d "lib" ]; then
    ls -la lib/
else
    echo "未找到lib目录"
fi

echo ""
echo "生成的可执行文件："
if [ -d "bin" ]; then
    ls -la bin/
else
    echo "未找到bin目录"
fi

echo ""

# 显示内存使用情况（如果可用）
if command -v du &> /dev/null; then
    echo "构建目录大小："
    du -sh .
fi

echo ""
echo "=================================================="
echo "完整构建测试成功完成！"
echo ""
echo "已构建的库："
for lib_name in "${lib_names[@]}"; do
    echo "- $lib_name"
done
echo "- rhfs90mpi (主程序)"
echo ""
echo "运行程序："
echo "cd $(pwd)"
echo "mpirun -np <进程数> bin/rhfs90mpi"
echo ""
echo "安装到项目目录："
echo "make install"
echo "=================================================="
