# RHFS_RIS_MPI

This project is a parallel version of the `rhfs90` program from the GRASP suite, adapted for MPI-based parallel execution. It was created by the Gemini code assistant.

## Build Instructions

This project uses CMake and requires a Fortran compiler, BLAS, LAPACK, and an MPI implementation (like OpenMPI or MPICH) to be installed on your system.

1.  **Create a build directory:**
    ```bash
    mkdir build
    cd build
    ```

2.  **Run CMake to configure the project:**
    ```bash
    cmake ..
    ```

3.  **Compile the code:**
    ```bash
    make
    ```
    The compiled binary `hfs92mpi` will be located in the `build/bin/` directory.

## Running the Program

To run the parallel program, use the `mpirun` command. You need to specify the number of processes with the `-np` flag.

For example, to run with 4 processes:
```bash
mpirun -np 4 ./bin/hfs92mpi
```

**Important:** Before running, ensure that all the necessary input files (`isodata`, `*.c`, `*.w`, etc.) are present in the directory where you execute the `mpirun` command. The program will read these files and create output files (e.g., `*.sum`, `*.h`) in the same directory.
