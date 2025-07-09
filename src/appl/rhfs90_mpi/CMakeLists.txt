add_executable(hfs92mpi
    hfs92mpi.f90
    mateltmpi.f90
    hfsggmpi.f90
    strsum_I.f90
    strsum.f90
    setsum_I.f90
    setsum.f90
    setdbg_I.f90
    setdbg.f90
    rinthf_I.f90
    rinthf.f90
    getmixblock_I.f90
    getmixblock.f90
    gethfd_I.f90
    gethfd.f90
    engouth_I.f90
    engouth.f90
    opt6_C.f90
)

target_link_libraries_Fortran(hfs92mpi PRIVATE
    mpi90
    lib9290
    libdvd90
    libmcp90
    librang90
    libmod
    ${LAPACK_LIBRARIES}
    ${BLAS_LIBRARIES}
)

install(TARGETS hfs92mpi DESTINATION bin)