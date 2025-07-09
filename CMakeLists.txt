cmake_minimum_required(VERSION 3.6)

project(rhfs_ris_mpi)

enable_language(Fortran)

if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release" CACHE STRING
        "Choose the type of build, options are: Release Debug."
        FORCE
    )
endif()

find_package(BLAS REQUIRED)
find_package(LAPACK REQUIRED)

set(Fortran_MODULE_DIRECTORY_root ${CMAKE_CURRENT_BINARY_DIR}/modules)
function(setup_fortran_modules target)
    set_property(TARGET ${target} PROPERTY Fortran_MODULE_DIRECTORY "${Fortran_MODULE_DIRECTORY_root}/${target}")
    install(DIRECTORY "${Fortran_MODULE_DIRECTORY_root}/${target}" DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/"
        FILES_MATCHING PATTERN "*.mod")
endfunction()
function(target_link_libraries_Fortran target mode)
    target_link_libraries(${target} ${mode} ${ARGN})
    foreach(lib IN LISTS ARGN)
        target_include_directories(${target} ${mode} $<TARGET_PROPERTY:${lib},Fortran_MODULE_DIRECTORY>)
    endforeach()
endfunction()

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/bin/")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/lib/")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/lib/")
set(CMAKE_INSTALL_PREFIX ${PROJECT_SOURCE_DIR})

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-automatic")

find_package(MPI REQUIRED)
if(MPI_Fortran_FOUND)
    message(STATUS "MPI Fortran found, building MPI-dependent libraries and programs.")
    add_subdirectory("src/lib/mpi90")
else()
    message(FATAL_ERROR "MPI Fortran libraries not found. This project requires MPI.")
endif()

add_subdirectory("src/lib/libmod")
add_subdirectory("src/lib/lib9290")
add_subdirectory("src/lib/libdvd90")
add_subdirectory("src/lib/libmcp90")
add_subdirectory("src/lib/librang90")

add_subdirectory("src/appl/rhfs90_mpi")
add_subdirectory("src/appl/ris4_mpi")
