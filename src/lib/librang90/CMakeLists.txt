add_library(librang90 STATIC
    angl.f90
    angl_I.f90
    biot.f90
    biot_I.f90
    ci.f90
    ci_I.f90
    coulomb.f90
    coulomb_I.f90
    gaunt.f90
    gaunt_I.f90
    laplace.f90
    laplace_I.f90
    gauge.f90
    gauge_I.f90
    qeda.f90
    qeda_I.f90
    qedb.f90
    qedb_I.f90
    qedc.f90
    qedc_I.f90
    qedd.f90
    qedd_I.f90
    se.f90
    se_I.f90
    vp.f90
    vp_I.f90
)
setup_fortran_modules(librang90)
target_link_libraries_Fortran(librang90 PRIVATE lib9290)
install(TARGETS librang90 DESTINATION lib)