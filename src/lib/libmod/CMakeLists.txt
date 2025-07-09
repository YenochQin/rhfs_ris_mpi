add_library(libmod STATIC
    buffer_C.f90
    core_C.f90
    debug_C.f90
    decide_C.f90
    def_C.f90
    eigv_C.f90
    foparm_C.f90
    grid_C.f90
    header_C.f90
    iounit_C.f90
    jlabl_C.f90
    memory_man.f90
    mpi_C.f90
    mpi_s.f90
    npar_C.f90
    nsmdat_C.f90
    orb_C.f90
    parameter_def.f90
    prnt_C.f90
    ris_C.f90
    syma_C.f90
    teilst_C.f90
    default_C.f90
)
setup_fortran_modules(libmod)
install(TARGETS libmod DESTINATION lib)