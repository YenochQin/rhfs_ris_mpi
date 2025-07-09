# RHFS90_MPI - MPI parallel version of RHFS90
# Calculates hyperfine structure parameters using parallel computation

add_executable(rhfs90mpi
    rhfs90mpi.f90
    hfsggmpi.f90
    hfsggmpi_I.f90
)

setup_fortran_modules(rhfs90mpi)

target_link_libraries_Fortran(rhfs90mpi PRIVATE 
    mod 
    9290 
    mpiu90
)

target_include_directories(rhfs90mpi PRIVATE ${MPI_Fortran_INCLUDE_PATH})
target_link_libraries(rhfs90mpi PRIVATE ${MPI_Fortran_LIBRARIES})

set_target_properties(rhfs90mpi PROPERTIES
    COMPILE_FLAGS "${MPI_Fortran_COMPILE_FLAGS}"
    LINK_FLAGS "${MPI_Fortran_LINK_FLAGS}"
)

install(TARGETS rhfs90mpi DESTINATION bin/)
