add_library(libmcp90 STATIC
    cxk.f90
    cxk_I.f90
    talk.f90
    talk_I.f90
)
setup_fortran_modules(libmcp90)
target_link_libraries_Fortran(libmcp90 PRIVATE lib9290)
install(TARGETS libmcp90 DESTINATION lib)