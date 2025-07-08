# GRASP RHFS/RIS MPI version - CMake Build System
#
# To build the CMake-based MPI version you first need to set up the build directory
# (out of tree builds are preferred). For that run:
#
#    mkdir build/ && cd build/ && cmake ..
#
# And then under the build/ directory simply call
#
#    make
#
# which will compile and install all the libraries to lib/ and executables to bin/
#

# This minimum CMake requirement can probably be loosened. But this code has not
# been tested with CMake versions older than 3.6.3.
cmake_minimum_required(VERSION 3.6)

project(GRASP_MPI)

enable_language(Fortran)
enable_testing()

# "Release" will be the default build type, which gives us optimization flags etc.
# The other relevant option would be "Debug", which disables optimizations and
# enables debugging symbols. The debug build can be enabled when setting up the
# build directory with CMake:
#
#     cmake -DCMAKE_BUILD_TYPE=Debug ..
#
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release" CACHE STRING
        "Choose the type of build, options are: Release Debug."
        FORCE
    )
endif(NOT CMAKE_BUILD_TYPE)

# Find required packages
find_package(BLAS REQUIRED)
find_package(LAPACK REQUIRED)
find_package(MPI REQUIRED)

# Check that MPI Fortran is available
if(NOT MPI_Fortran_FOUND)
    message(FATAL_ERROR "MPI Fortran compiler not found. This is an MPI-only project.")
endif()

# We need special functions to handle linking Fortran modules between libraries
# etc. The Fortran_MODULE_DIRECTORY_root variable is the directory where all the
# .mod files get written to. It is set be the modules/ subdirectory of the build
# directory.
#
# For every library, the modules get stored in
# ${Fortran_MODULE_DIRECTORY_root}/<library_name>/ so the modules from different
# libraries are separated from each other.
#
# setup_fortran_modules(target)
# Needs to be called on all libraries that provide modules.
#
# target_link_libraries_Fortran(target mode libraries...)
# Similar to target_link_libraries(), but will also set up paths so that the
# compiler could find the Fortran .mod files from the libraries.
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

# We put the compiled binaries into the bin/ subdirectory of the build directory
# and libraries into the lib/ subdirectory.
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/bin/")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/lib/")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/lib/")
# To install the binaries into the standard <repo>/bin/ directory, you need to
# call `make install`.
set(CMAKE_INSTALL_PREFIX ${PROJECT_SOURCE_DIR})

# Additional Fortran compiler flags.
# -fno-automatic: this was set in the original make_environment_gfortran_UBC file.
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-automatic")

# MPI Fortran compiler flags
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${MPI_Fortran_COMPILE_FLAGS}")

message("Compiler flags etc. for this GRASP MPI build:")
message("* CMAKE_BUILD_TYPE:                ${CMAKE_BUILD_TYPE}")
message("* CMAKE_Fortran_COMPILER:          ${CMAKE_Fortran_COMPILER}")
message("* CMAKE_Fortran_COMPILER_VERSION:  ${CMAKE_Fortran_COMPILER_VERSION}")
message("* CMAKE_Fortran_FLAGS:             ${CMAKE_Fortran_FLAGS}")
message("* CMAKE_Fortran_FLAGS_RELEASE:     ${CMAKE_Fortran_FLAGS_RELEASE}")
message("* CMAKE_Fortran_FLAGS_DEBUG:       ${CMAKE_Fortran_FLAGS_DEBUG}")
message("* CMAKE_EXE_LINKER_FLAGS:          ${CMAKE_EXE_LINKER_FLAGS}")
message("* CMAKE_STATIC_LINKER_FLAGS:       ${CMAKE_STATIC_LINKER_FLAGS}")
message("* CMAKE_RUNTIME_OUTPUT_DIRECTORY:  ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")
message("* CMAKE_LIBRARY_OUTPUT_DIRECTORY:  ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}")
message("* CMAKE_ARCHIVE_OUTPUT_DIRECTORY:  ${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}")
message("* CMAKE_POSITION_INDEPENDENT_CODE: ${CMAKE_POSITION_INDEPENDENT_CODE}")
message("* BLAS_LIBRARIES:                  ${BLAS_LIBRARIES}")
message("* BLAS_LINKER_FLAGS:               ${BLAS_LINKER_FLAGS}")
message("* LAPACK_LIBRARIES:                ${LAPACK_LIBRARIES}")
message("* LAPACK_LINKER_FLAGS:             ${LAPACK_LINKER_FLAGS}")
message("* MPI_Fortran_LIBRARIES:           ${MPI_Fortran_LIBRARIES}")
message("* MPI_Fortran_INCLUDE_PATH:        ${MPI_Fortran_INCLUDE_PATH}")
message("* MPI_Fortran_COMPILE_FLAGS:       ${MPI_Fortran_COMPILE_FLAGS}")
message("* MPI_Fortran_LINK_FLAGS:          ${MPI_Fortran_LINK_FLAGS}")

# GRASP MPI libraries
add_subdirectory("src/lib/libmod")
add_subdirectory("src/lib/lib9290")
add_subdirectory("src/lib/libdvd90")
add_subdirectory("src/lib/libmcp90")
add_subdirectory("src/lib/librang90")
add_subdirectory("src/lib/mpi90")

# GRASP MPI application programs
add_subdirectory("src/rhfs90_mpi")
add_subdirectory("src/rhfszeeman95_mpi")
add_subdirectory("src/ris4_mpi")

# Set the GRASP_MPI variable that external targets can use
set(GRASP_MPI ${PROJECT_SOURCE_DIR})
unset(GRASP_MPI_CMakeLists_user CACHE)
find_file(GRASP_MPI_CMakeLists_user "CMakeLists.user" ${PROJECT_SOURCE_DIR})
if(NOT "${GRASP_MPI_CMakeLists_user}" STREQUAL "GRASP_MPI_CMakeLists_user-NOTFOUND")
    include(${GRASP_MPI_CMakeLists_user})
endif() 