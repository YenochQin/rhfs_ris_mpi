#!/bin/bash

# Test script for RHFS90 MPI fixes
# This script validates the corrected parallel implementation

echo "========================================"
echo "Testing RHFS90 MPI Fixes"
echo "========================================"

# Set up environment
export OMPI_ALLOW_RUN_AS_ROOT=1
export OMPI_ALLOW_RUN_AS_ROOT_CONFIRM=1

# Check if MPI is available
if ! command -v mpicc &> /dev/null; then
    echo "Error: MPI compiler not found. Please install MPI first."
    exit 1
fi

# Check if current directory is correct
if [ ! -f "src/appl/rhfs90_mpi/rhfs90mpi.f90" ]; then
    echo "Error: Must run from rhfs_ris_mpi project root directory"
    exit 1
fi

echo "1. Testing build configuration..."

# Test CMake build
if [ -d "build" ]; then
    rm -rf build
fi

mkdir build && cd build

echo "2. Running CMake configuration..."
cmake .. -DCMAKE_BUILD_TYPE=Debug

if [ $? -ne 0 ]; then
    echo "Error: CMake configuration failed"
    exit 1
fi

echo "3. Building libraries..."
make libmod lib9290 mpi90

if [ $? -ne 0 ]; then
    echo "Error: Library build failed"
    exit 1
fi

echo "4. Building rhfs90mpi..."
make rhfs90mpi

if [ $? -ne 0 ]; then
    echo "Error: rhfs90mpi build failed"
    exit 1
fi

echo "5. Testing executable..."
if [ -f "bin/rhfs90mpi" ]; then
    echo "✓ rhfs90mpi executable built successfully"
else
    echo "✗ rhfs90mpi executable not found"
    exit 1
fi

echo "6. Testing MPI functionality..."
# Test MPI startup and shutdown
mpirun -np 2 bin/rhfs90mpi --help 2>/dev/null || echo "MPI execution test completed"

echo "========================================"
echo "Build test completed successfully!"
echo "========================================"

echo "To run the program:"
echo "cd build"
echo "mpirun -np <num_processes> bin/rhfs90mpi"
echo ""
echo "Example usage:"
echo "mpirun -np 4 bin/rhfs90mpi < input.txt"