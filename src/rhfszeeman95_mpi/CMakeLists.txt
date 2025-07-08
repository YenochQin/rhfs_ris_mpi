add_executable(rhfszeeman95_mpi
    opt6_C.f90
    engouth_I.f90
    gethfd_I.f90
    hfszeemanmpi_I.f90
    matelt_I.f90
    rinthf_I.f90
    setdbg_I.f90
    setsum_I.f90
    strsum_I.f90
    getmixblock_I.f90
    engouth.f90
    gethfd.f90

    hfszeeman06mpi.f90
    matelt.f90
    rinthf.f90
    setdbg.f90
    setsum.f90
    strsum.f90
    getmixblock.f90
)
target_link_libraries_Fortran(rhfszeeman95_mpi PUBLIC mpiu90 rang90 mcp90 9290 mod)
target_include_directories(rhfszeeman95_mpi PRIVATE ${MPI_Fortran_INCLUDE_PATH})
target_link_libraries(rhfszeeman95_mpi PRIVATE ${MPI_Fortran_LIBRARIES})
set_target_properties(rhfszeeman95_mpi PROPERTIES
  COMPILE_FLAGS "${MPI_Fortran_COMPILE_FLAGS}"
  LINK_FLAGS "${MPI_Fortran_LINK_FLAGS}"
)
install(TARGETS rhfszeeman95_mpi DESTINATION bin/)
