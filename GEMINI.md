# Gemini Project Modifications

This file documents the changes made by the Gemini code assistant to create the `rhfs_ris_mpi` project.

**Project Goal:** To create an MPI-parallel version of the `rhfs90` program from the GRASP suite, following the existing parallelization patterns found in `grasp`.

## Summary of Changes:

1.  **Project Scaffolding:**
    *   A new directory `rhfs_ris_mpi` was created.
    *   The original `rhfs90` application from `grasp/src/appl/rhfs90` was copied to `rhfs_ris_mpi/src/appl/rhfs90_mpi`.
    *   All required libraries from `grasp/src/lib` (`lib9290`, `libdvd90`, `libmcp90`, `librang90`, `libmod`, and `mpi90`) were copied into `rhfs_ris_mpi/src/lib`.

2.  **Code Modifications (Parallelization):**
    *   **File Renaming:**
        *   `hfs92.f90` -> `hfs92mpi.f90` (Main program)
        *   `hfsgg.f90` -> `hfsggmpi.f90` (Core computation subroutine)
        *   `matelt.f90` -> `mateltmpi.f90` (Angular computation, renamed for consistency)
    *   **Main Program (`hfs92mpi.f90`):**
        *   Integrated `startmpi2` and `stopmpi2` from the `mpi90` library to manage the MPI environment.
        *   Restricted all file I/O operations (reading inputs, writing outputs) to the root process (`myid == 0`).
        *   Input data read by the root process is now broadcast to all other processes using `MPI_Bcast`.
    *   **Computation Subroutine (`hfsggmpi.f90`):**
        *   The main computational loop over `NCF` (number of configurations) was parallelized. The loop `DO IC = 1, NCF` was changed to `DO IC = myid+1, NCF, nprocs`, effectively distributing the workload across all available MPI processes.
        *   After the parallel loop, a call to `gdsummpi` (an `MPI_Allreduce` wrapper from the `mpi90` library) was added to sum the partial results of the Hamiltonian matrix (`HFC`) and `g_j` factor matrices (`GJC`, `DGJC`) from all processes into a final, globally consistent matrix.

3.  **Build System:**
    *   A new top-level `CMakeLists.txt` was created for the `rhfs_ris_mpi` project.
    *   A new `CMakeLists.txt` was created in the `src/appl/rhfs90_mpi` directory to define the `hfs92mpi` executable and link it against all required libraries.

This approach mirrors the established parallelization strategy of the `grasp` project, ensuring consistency and maintainability.
