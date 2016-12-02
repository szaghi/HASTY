#!/usr/bin/make

# defaults
COMPILER = gnu
DEBUG = yes
CAF = yes

#main building variables
DOBJ = exe/obj/
DMOD = exe/mod/
DEXE = exe/
DSRC = src/

DEBUG_GNU =
DEBUG_INT =
ifeq "$(DEBUG)" "yes"
  DEBUG_GNU = -Og -g3 -Wall -Wno-surprising -Wextra -fcheck=all -fbacktrace -std=f2008 -fall-intrinsics
  DEBUG_INT = -O0 -debug all -check all -warn all -traceback -std08 -standard-semantics
endif

FC_GNU = gfortran
FC_INT = ifort
OPTSC_CAF_GNU =
OPTSC_CAF_INT =
OPTSL_CAF_GNU =
OPTSL_CAF_INT =
ifeq "$(CAF)" "yes"
  FC_GNU = caf
  FC_INT = ifort
  OPTSC_CAF_GNU = -fcoarray=lib -DCAF
  OPTSC_CAF_INT = -coarray -DCAF
  OPTSL_CAF_GNU = -fcoarray=lib -lcaf_mpi
  OPTSL_CAF_INT = -coarray -coarray
endif

ifeq "$(COMPILER)" "gnu"
  FC    = $(FC_GNU)
  OPTSC = -c -frealloc-lhs -J $(DMOD) $(DEBUG_GNU) $(OPTSC_CAF_GNU)
  OPTSL = -J $(DMOD) $(OPTSL_CAF_GNU)
endif
ifeq "$(COMPILER)" "intel"
  FC    = $(FC_INT)
  OPTSC = -c -assume realloc_lhs -module $(DMOD) $(DEBUG_INT) $(OPTSC_CAF_INT)
  OPTSL = -module $(DMOD) $(OPTSL_CAF_INT)
endif

VPATH   = $(DSRC) $(DOBJ) $(DMOD)
MKDIRS  = $(DOBJ) $(DMOD) $(DEXE)
LCEXES  = $(shell echo $(EXES) | tr '[:upper:]' '[:lower:]')
EXESPO  = $(addsuffix .o,$(LCEXES))
EXESOBJ = $(addprefix $(DOBJ),$(EXESPO))

#auxiliary variables
COTEXT = "Compile $(<F)"
LITEXT = "Assemble $@"
RUTEXT = "Executed rule $@"

firsrule: $(DEXE)HASTY_TEST_CAF_GET_CLONE

#building rules
$(DEXE)HASTY_TEST_CAF_GET_CLONE: $(MKDIRS) $(DOBJ)hasty_test_caf_get_clone.o
	@rm -f $(filter-out $(DOBJ)hasty_test_caf_get_clone.o,$(EXESOBJ))
	@echo $(LITEXT)
	@$(FC) $(OPTSL) $(DOBJ)*.o $(LIBS) -o $@
EXES := $(EXES) HASTY_TEST_CAF_GET_CLONE

#compiling rules
$(DOBJ)hasty_key_morton.o: src/lib/hasty_key_morton.f90 \
	$(DOBJ)hasty_key_base.o \
	$(DOBJ)penf.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)hasty_hash_table.o: src/lib/hasty_hash_table.F90 \
	$(DOBJ)hasty_content_adt.o \
	$(DOBJ)hasty_key_base.o \
	$(DOBJ)hasty_dictionary.o \
	$(DOBJ)penf.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)hasty_key_base.o: src/lib/hasty_key_base.f90 \
	$(DOBJ)penf.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)hasty_content_adt.o: src/lib/hasty_content_adt.f90
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)hasty_dictionary_node.o: src/lib/hasty_dictionary_node.f90 \
	$(DOBJ)hasty_content_adt.o \
	$(DOBJ)hasty_key_base.o \
	$(DOBJ)penf.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)hasty.o: src/lib/hasty.f90 \
	$(DOBJ)hasty_content_adt.o \
	$(DOBJ)hasty_key_base.o \
	$(DOBJ)hasty_key_morton.o \
	$(DOBJ)hasty_hash_table.o \
	$(DOBJ)hasty_dictionary.o \
	$(DOBJ)hasty_dictionary_node.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)hasty_dictionary.o: src/lib/hasty_dictionary.f90 \
	$(DOBJ)hasty_key_base.o \
	$(DOBJ)hasty_dictionary_node.o \
	$(DOBJ)penf.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)tester.o: src/third_party/fortran_tester/src/tester.f90
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)penf_b_size.o: src/third_party/PENF/src/lib/penf_b_size.F90 \
	$(DOBJ)penf_global_parameters_variables.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)penf.o: src/third_party/PENF/src/lib/penf.F90 \
	$(DOBJ)penf_global_parameters_variables.o \
	$(DOBJ)penf_b_size.o \
	$(DOBJ)penf_stringify.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)penf_stringify.o: src/third_party/PENF/src/lib/penf_stringify.F90 \
	$(DOBJ)penf_b_size.o \
	$(DOBJ)penf_global_parameters_variables.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)penf_global_parameters_variables.o: src/third_party/PENF/src/lib/penf_global_parameters_variables.F90
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

$(DOBJ)hasty_test_caf_get_clone.o: src/tests/hasty_test_caf_get_clone.F90 \
	$(DOBJ)hasty.o
	@echo $(COTEXT)
	@$(FC) $(OPTSC)  $< -o $@

#phony auxiliary rules
.PHONY : $(MKDIRS)
$(MKDIRS):
	@mkdir -p $@
.PHONY : cleanobj
cleanobj:
	@echo deleting objects
	@rm -fr $(DOBJ)
.PHONY : cleanmod
cleanmod:
	@echo deleting mods
	@rm -fr $(DMOD)
.PHONY : cleanexe
cleanexe:
	@echo deleting exes
	@rm -f $(addprefix $(DEXE),$(EXES))
.PHONY : clean
clean: cleanobj cleanmod
.PHONY : cleanall
cleanall: clean cleanexe
