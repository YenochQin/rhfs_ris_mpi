#***************************************************************************
#
#               Makefile for RHFS, RHFSZEEMAN95, and RIS4 MPI versions
#
#***************************************************************************

# Compiler and flags
FC = mpif90
FCFLAGS = -O2 -cpp -ffree-line-length-none

# Directories
SRCDIR = src
LIBDIR = $(SRCDIR)/lib
BINDIR = bin
INCDIR = $(LIBDIR)

# Library directories
LIB9290DIR = $(LIBDIR)/lib9290
LIBMODDIR = $(LIBDIR)/libmod
LIBDVD90DIR = $(LIBDIR)/libdvd90
LIBMCP90DIR = $(LIBDIR)/libmcp90
LIBRANG90DIR = $(LIBDIR)/librang90
MPI90DIR = $(LIBDIR)/mpi90

# Application directories
RHFS90MPIDIR = $(SRCDIR)/rhfs90_mpi
RHFSZEEMAN95MPIDIR = $(SRCDIR)/rhfszeeman95_mpi
RIS4MPIDIR = $(SRCDIR)/ris4_mpi

# Include paths
INCLUDES = -I$(LIB9290DIR) -I$(LIBMODDIR) -I$(LIBDVD90DIR) -I$(LIBMCP90DIR) -I$(LIBRANG90DIR) -I$(MPI90DIR)

# Library objects
LIB9290_OBJS = $(wildcard $(LIB9290DIR)/*.f90)
LIBMOD_OBJS = $(wildcard $(LIBMODDIR)/*.f90)
LIBDVD90_OBJS = $(wildcard $(LIBDVD90DIR)/*.f90)
LIBMCP90_OBJS = $(wildcard $(LIBMCP90DIR)/*.f90)
LIBRANG90_OBJS = $(wildcard $(LIBRANG90DIR)/*.f90)
MPI90_OBJS = $(wildcard $(MPI90DIR)/*.f90)

# All library objects
ALL_LIB_OBJS = $(LIB9290_OBJS) $(LIBMOD_OBJS) $(LIBDVD90_OBJS) $(LIBMCP90_OBJS) $(LIBRANG90_OBJS) $(MPI90_OBJS)

# Application objects
RHFS90MPI_OBJS = $(wildcard $(RHFS90MPIDIR)/*.f90)
RHFSZEEMAN95MPI_OBJS = $(wildcard $(RHFSZEEMAN95MPIDIR)/*.f90)
RIS4MPI_OBJS = $(wildcard $(RIS4MPIDIR)/*.f90)

# Target executables
RHFS90MPI_EXE = $(BINDIR)/rhfs90mpi
RHFSZEEMAN95MPI_EXE = $(BINDIR)/rhfszeeman95mpi
RIS4MPI_EXE = $(BINDIR)/ris4mpi

# Default target
all: $(RHFS90MPI_EXE) $(RHFSZEEMAN95MPI_EXE) $(RIS4MPI_EXE)

# Create bin directory
$(BINDIR):
	mkdir -p $(BINDIR)

# RHFS90 MPI executable
$(RHFS90MPI_EXE): $(BINDIR) $(ALL_LIB_OBJS) $(RHFS90MPI_OBJS)
	$(FC) $(FCFLAGS) $(INCLUDES) -o $@ \
		$(ALL_LIB_OBJS) \
		$(RHFS90MPIDIR)/hfs92mpi.f90 \
		$(RHFS90MPIDIR)/hfsggmpi.f90 \
		$(RHFS90MPIDIR)/hfsggmpi_I.f90 \
		$(filter-out $(RHFS90MPIDIR)/hfs92mpi.f90 $(RHFS90MPIDIR)/hfsggmpi.f90 $(RHFS90MPIDIR)/hfsggmpi_I.f90, $(RHFS90MPI_OBJS))

# RHFSZEEMAN95 MPI executable  
$(RHFSZEEMAN95MPI_EXE): $(BINDIR) $(ALL_LIB_OBJS) $(RHFSZEEMAN95MPI_OBJS)
	$(FC) $(FCFLAGS) $(INCLUDES) -o $@ \
		$(ALL_LIB_OBJS) \
		$(RHFSZEEMAN95MPIDIR)/hfszeeman06mpi.f90 \
		$(RHFSZEEMAN95MPIDIR)/hfszeemanmpi_I.f90 \
		$(filter-out $(RHFSZEEMAN95MPIDIR)/hfszeeman06mpi.f90 $(RHFSZEEMAN95MPIDIR)/hfszeemanmpi_I.f90, $(RHFSZEEMAN95MPI_OBJS))

# RIS4 MPI executable
$(RIS4MPI_EXE): $(BINDIR) $(ALL_LIB_OBJS) $(RIS4MPI_OBJS)
	$(FC) $(FCFLAGS) $(INCLUDES) -o $@ \
		$(ALL_LIB_OBJS) \
		$(RIS4MPIDIR)/rismpi.f90 \
		$(RIS4MPIDIR)/ris_cal_mpi_I.f90 \
		$(filter-out $(RIS4MPIDIR)/rismpi.f90 $(RIS4MPIDIR)/ris_cal_mpi_I.f90, $(RIS4MPI_OBJS))

# Individual targets
rhfs90mpi: $(RHFS90MPI_EXE)
rhfszeeman95mpi: $(RHFSZEEMAN95MPI_EXE)
ris4mpi: $(RIS4MPI_EXE)

# Clean target
clean:
	rm -f $(BINDIR)/*
	find $(SRCDIR) -name "*.mod" -delete
	find $(SRCDIR) -name "*.o" -delete

# Install target
install: all
	@echo "MPI executables built successfully:"
	@echo "  $(RHFS90MPI_EXE)"
	@echo "  $(RHFSZEEMAN95MPI_EXE)" 
	@echo "  $(RIS4MPI_EXE)"
	@echo ""
	@echo "To run with MPI, use:"
	@echo "  mpirun -np <num_processes> $(RHFS90MPI_EXE)"
	@echo "  mpirun -np <num_processes> $(RHFSZEEMAN95MPI_EXE)"
	@echo "  mpirun -np <num_processes> $(RIS4MPI_EXE)"

# Help target
help:
	@echo "Available targets:"
	@echo "  all              - Build all MPI executables"
	@echo "  rhfs90mpi        - Build RHFS90 MPI version"
	@echo "  rhfszeeman95mpi  - Build RHFSZEEMAN95 MPI version"
	@echo "  ris4mpi          - Build RIS4 MPI version"
	@echo "  clean            - Remove built files"
	@echo "  install          - Build and show usage instructions"
	@echo "  help             - Show this help message"

.PHONY: all clean install help rhfs90mpi rhfszeeman95mpi ris4mpi 