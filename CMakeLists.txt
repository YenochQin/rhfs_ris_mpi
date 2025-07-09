cmake_minimum_required(VERSION 3.6)

project(rhfs_ris_mpi)

#-------------------------------------------------------------------------------
# Global Project Setup
#-------------------------------------------------------------------------------
enable_language(Fortran)

# Default to "Release" build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release" CACHE STRING
        "Choose the type of build, options are: Release Debug."
        FORCE
    )
endif()

# Find external libraries
find_package(BLAS REQUIRED)
find_package(LAPACK REQUIRED)
find_package(MPI REQUIRED Fortran)

# Set output directories for binaries and libraries
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
set(CMAKE_INSTALL_PREFIX ${PROJECT_SOURCE_DIR})

# Set global Fortran flags
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-automatic")
# The following flag may be needed for some gfortran versions
# set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch")

#-------------------------------------------------------------------------------
# Custom Functions for Fortran Module Management
# (Copied from the GRASP project)
#-------------------------------------------------------------------------------

# Root directory for all .mod files
set(Fortran_MODULE_DIRECTORY_root ${CMAKE_CURRENT_BINARY_DIR}/modules)

# This function sets the module output directory for a given library target.
function(setup_fortran_modules target)
    set_property(TARGET ${target} PROPERTY Fortran_MODULE_DIRECTORY "${Fortran_MODULE_DIRECTORY_root}/${target}")
    install(DIRECTORY "${Fortran_MODULE_DIRECTORY_root}/${target}" DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/"
        FILES_MATCHING PATTERN "*.mod")
endfunction()

# This function links libraries and automatically adds the necessary include
# directories for their Fortran modules.
function(target_link_libraries_Fortran target mode)
    target_link_libraries(${target} ${mode} ${ARGN})
    foreach(lib IN LISTS ARGN)
        # Only try to get module directories from actual CMake targets
        if(TARGET ${lib})
            target_include_directories(${target} ${mode} $<TARGET_PROPERTY:${lib},Fortran_MODULE_DIRECTORY>)
        endif()
    endforeach()
endfunction()

#-------------------------------------------------------------------------------
# Add Subdirectories in Correct Dependency Order
#-------------------------------------------------------------------------------

# 1. Add libraries that provide modules but have few dependencies
add_subdirectory("src/lib/libmod")
add_subdirectory("src/lib/lib9290")

# 2. Add libraries that depend on the above
add_subdirectory("src/lib/libdvd90")
add_subdirectory("src/lib/libmcp90")
add_subdirectory("src/lib/librang90")
add_subdirectory("src/lib/mpi90")

# 3. Add the main applications, which depend on all the libraries
add_subdirectory("src/appl/rhfs90_mpi")
add_subdirectory("src/appl/ris4_mpi")

message(STATUS "Configuration complete. You can now run 'make' to build the project.")