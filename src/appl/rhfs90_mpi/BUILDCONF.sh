#!/bin/bash
#
# Build configuration script for RHFS90_MPI
# Based on GRASP build system
#

# Fortran compiler flags for MPI
FFLAGS="-O2 -fPIC"
MPIFC="mpif90"

# Library dependencies
LIBS="-L../../lib/lib9290 -l9290 -L../../lib/libmod -lmod -L../../lib/mpi90 -lmpiu90"

# Include directories
INCLUDES="-I../../lib/libmod -I../../lib/lib9290 -I../../lib/mpi90"

echo "Building RHFS90_MPI with MPI support..."
echo "Compiler: $MPIFC"
echo "Flags: $FFLAGS"
echo "Libraries: $LIBS"
echo "Includes: $INCLUDES"

# Compile
$MPIFC $FFLAGS $INCLUDES -c hfsggmpi_I.f90
$MPIFC $FFLAGS $INCLUDES -c hfsggmpi.f90
$MPIFC $FFLAGS $INCLUDES -c rhfs90mpi.f90

# Link
$MPIFC $FFLAGS -o rhfs90mpi rhfs90mpi.o hfsggmpi.o hfsggmpi_I.o $LIBS

echo "Build completed: rhfs90mpi"
