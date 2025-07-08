#!/bin/bash

#***************************************************************************
#
#               Test script for RHFS/RIS MPI Implementation
#
#***************************************************************************

echo "========================================"
echo "Testing RHFS/RIS MPI Implementation"
echo "========================================"

# Check if required tools are available
echo "Checking required tools..."

# Check for MPI
if ! command -v mpirun &> /dev/null; then
    echo "ERROR: mpirun not found. Please install OpenMPI or MPICH."
    exit 1
fi

if ! command -v mpif90 &> /dev/null; then
    echo "ERROR: mpif90 not found. Please install MPI Fortran compiler."
    exit 1
fi

echo "✓ MPI tools found"

# Check for gfortran
if ! command -v gfortran &> /dev/null; then
    echo "ERROR: gfortran not found. Please install gfortran."
    exit 1
fi

echo "✓ gfortran found"

# Display MPI information
echo ""
echo "MPI Configuration:"
mpirun --version | head -1
mpif90 --version | head -1

echo ""
echo "========================================"
echo "Building MPI programs..."
echo "========================================"

# Clean previous builds
echo "Cleaning previous builds..."
make clean

# Build all programs
echo "Building all MPI programs..."
make all

# Check if executables were created
echo ""
echo "Checking built executables:"

if [ -f "bin/rhfs90mpi" ]; then
    echo "✓ rhfs90mpi executable created"
    ls -la bin/rhfs90mpi
else
    echo "✗ rhfs90mpi executable NOT found"
    exit 1
fi

if [ -f "bin/rhfszeeman95mpi" ]; then
    echo "✓ rhfszeeman95mpi executable created"
    ls -la bin/rhfszeeman95mpi
else
    echo "✗ rhfszeeman95mpi executable NOT found"
    exit 1
fi

if [ -f "bin/ris4mpi" ]; then
    echo "✓ ris4mpi executable created"
    ls -la bin/ris4mpi
else
    echo "✗ ris4mpi executable NOT found"
    exit 1
fi

echo ""
echo "========================================"
echo "Testing basic MPI functionality..."
echo "========================================"

# Test MPI basic functionality with a simple command
echo "Testing MPI with 2 processes:"
mpirun -np 2 echo "Hello from MPI process"

if [ $? -eq 0 ]; then
    echo "✓ MPI basic test passed"
else
    echo "✗ MPI basic test failed"
    exit 1
fi

echo ""
echo "========================================"
echo "Build and test completed successfully!"
echo "========================================"

echo ""
echo "Next steps:"
echo "1. Prepare input files (isodata, name.c, name.w, name.cm)"
echo "2. Run programs with:"
echo "   mpirun -np 4 ./bin/rhfs90mpi"
echo "   mpirun -np 4 ./bin/rhfszeeman95mpi"
echo "   mpirun -np 4 ./bin/ris4mpi"
echo ""
echo "For more information, see README.md"

exit 0 