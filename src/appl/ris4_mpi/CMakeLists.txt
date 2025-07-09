add_executable(rismpi
    rismpi.f90
    ris_cal.f90
    angdata.f90
    densnew.f90
    densnew_seltz.f90
    densread.f90
    densread_seltz.f90
    edensityfit.f90
    engout.f90
    fical.f90
    getmixblock.f90
    getsmd.f90
    polint.f90
    rint_sms2.f90
    rint_sms3.f90
    rintdens.f90
    rintdensvec.f90
    rinti_nms.f90
    setdbg.f90
    setsum.f90
    sigma_1.f90
    sigma_2.f90
    smsnew.f90
    smsread.f90
    strsum.f90
    vinti.f90
    ris_C.f90
    teilst_C.f90
)

target_link_libraries_Fortran(rismpi PRIVATE
    mpi90
    lib9290
    libdvd90
    libmcp90
    librang90
    libmod
    ${LAPACK_LIBRARIES}
    ${BLAS_LIBRARIES}
)

install(TARGETS rismpi DESTINATION bin)