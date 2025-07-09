add_library(libmcp90 STATIC
    alcmc1.f90
    alcmc1_I.f90
    alcmc2.f90
    alcmc2_I.f90
    lodmcp.f90
    lodmcp_I.f90
    setmcp1.f90
    setmcp1_I.f90
    setmcp2.f90
    setmcp2_I.f90
)
setup_fortran_modules(libmcp90)
target_link_libraries_Fortran(libmcp90 PRIVATE lib9290)
install(TARGETS libmcp90 DESTINATION lib)