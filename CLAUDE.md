# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

RHFS_RIS_MPI is a MPI-parallelized version of the RHFS90 hyperfine structure calculation program from the GRASP (General-purpose Relativistic Atomic Structure Package) suite. This project focuses on parallel computation of atomic hyperfine structure parameters using MPI.

## Build System

### CMake (Primary Build System)
```bash
# Full build from project root
mkdir build && cd build
cmake ..
make rhfs90mpi

# Or build specific target
make rhfs90mpi

# Install to bin/
make install
```

### Test Build
```bash
# Automated build verification
./test_cmake_build.sh

# Alternative configuration script
./configure.sh
```

### Build Dependencies
- MPI libraries (OpenMPI, MPICH, etc.)
- Fortran compiler with MPI support
- BLAS/LAPACK libraries
- CMake 3.6+

## Code Architecture

### Main Application Structure
- **Main program**: `src/appl/rhfs90_mpi/rhfs90mpi.f90` - MPI main entry point
- **Core computation**: `src/appl/rhfs90_mpi/hfsggmpi.f90` - Parallelized hyperfine structure calculation
- **Interface files**: `*_I.f90` files provide module interfaces

### Library Organization
The project uses a modular library structure:
- `src/lib/libmod/` - Core GRASP modules and common definitions
- `src/lib/lib9290/` - GRASP utility functions
- `src/lib/libdvd90/` - Davidson diagonalization routines
- `src/lib/libmcp90/` - Multi-configuration packages
- `src/lib/librang90/` - Angular momentum coupling routines
- `src/lib/mpi90/` - MPI utility functions

### Parallelization Strategy
The MPI parallelization distributes work across configuration state functions:
- Work distribution: Each process handles `IC = myid+1, myid+1+nprocs, myid+1+2*nprocs, ...`
- Results aggregation: Uses `MPI_ALLREDUCE` for collecting distributed calculations
- File I/O: Only rank 0 process handles output file generation

## Development Commands

### Building and Testing
```bash
# Build entire project
mkdir build && cd build
cmake ..
make

# Build only rhfs90mpi
make rhfs90mpi

# Run automated tests
./test_cmake_build.sh
./test_all_libs_build.sh

# Clean build
rm -rf build
```

### Running the Program
```bash
# Execute with MPI
mpirun -np <num_processes> rhfs90mpi

# From build directory
cd build
mpirun -np 4 bin/rhfs90mpi
```

## Code Conventions

### Fortran Standards
- Uses Fortran 90+ with modules
- Interface files (`*_I.f90`) define module interfaces
- Module files (`*_C.f90`) contain common data structures
- Compilation flag: `-fno-automatic -fallow-argument-mismatch`

### File Organization
- Each source file has corresponding interface file
- Libraries are organized in separate directories under `src/lib/`
- Build configuration in individual `CMakeLists.txt` files
- Build scripts (`BUILDCONF.sh`) in each library directory

### MPI Integration
- Uses custom MPI wrapper functions from `mpi90` library
- MPI initialization/finalization handled by `startmpi2`/`stopmpi2`
- Process identification through `myid` and `nprocs` variables

## Key Technical Notes

### Memory Management
- Uses dynamic memory allocation through `memory_man` module
- Fortran modules handle automatic memory management
- Large arrays distributed across MPI processes

### Input/Output
- Reads standard GRASP input files (`.c`, `.cm`, `.w`, `isodata`)
- Generates hyperfine structure output files (`.h`, `.ch`, `.hoffd`)
- Only rank 0 process handles file output to prevent conflicts

### Performance Considerations
- Recommended process count: ≤ number of configuration state functions
- Optimal range: 16-64 processes for large calculations
- Load balancing through round-robin work distribution

## Dependencies and Libraries

### Required External Libraries
- MPI implementation (OpenMPI, MPICH, Intel MPI)
- BLAS/LAPACK for linear algebra operations
- Fortran compiler with MPI support

### Internal Library Dependencies
Libraries must be built in dependency order:
1. `libmod` (core modules)
2. `lib9290` (utilities)
3. `libdvd90`, `libmcp90`, `librang90` (specialized functions)
4. `mpi90` (MPI utilities)
5. `rhfs90_mpi` (main application)

## Testing

### Verification Scripts
- `test_cmake_build.sh` - Comprehensive build verification
- `test_all_libs_build.sh` - Library-specific build testing
- Individual library test scripts in `src/lib/*/test_build.sh`

### Build Validation
The test scripts verify:
- Source file completeness
- MPI compiler availability
- Library dependencies
- Compilation success
- Basic functionality tests