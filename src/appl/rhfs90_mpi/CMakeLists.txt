# RHFS90_MPI - MPI parallel version of RHFS90
# Calculates hyperfine structure parameters using parallel computation

add_executable(rhfs90mpi
    rhfs90mpi.f90
    hfsggmpi.f90
    hfsggmpi_I.f90
    setdbg.f90
    setdbg_I.f90
    setsum.f90
    setsum_I.f90
    gethfd.f90
    gethfd_I.f90
    getmixblock.f90
    getmixblock_I.f90
    strsum.f90
    strsum_I.f90
    matelt.f90
    matelt_I.f90
    rinthf.f90
    rinthf_I.f90
    opt6_C.f90
    engouth.f90
    engouth_I.f90
)

setup_fortran_modules(rhfs90mpi)

target_link_libraries_Fortran(rhfs90mpi PRIVATE 
    mod 
    9290 
    rang90 
    mpiu90
)

target_include_directories(rhfs90mpi PRIVATE ${MPI_Fortran_INCLUDE_PATH})
target_link_libraries(rhfs90mpi PRIVATE ${MPI_Fortran_LIBRARIES})

set_target_properties(rhfs90mpi PROPERTIES
    COMPILE_FLAGS "${MPI_Fortran_COMPILE_FLAGS}"
    LINK_FLAGS "${MPI_Fortran_LINK_FLAGS}"
)

install(TARGETS rhfs90mpi DESTINATION bin/)
