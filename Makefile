# -*- mode: makefile -*-

FC:=gfortran
SHARED_LIB_EXT:=.so

FFLAGS+=-I. -shared -fPIC

all: libspecfun$(SHARED_LIB_EXT)

libspecfun$(SHARED_LIB_EXT): specfun.f
	$(FC) $(FFLAGS) -std=legacy -o $@ $^


%.exe: specfun.f %.f
	$(FC) -g -std=legacy -o $@ $^


dev: clean-dev dev.exe
	./dev.exe
specfun.test: clean-test specfun.test.exe
	./specfun.test.exe
test: specfun.test

clean-dev:
	-rm -f dev.exe
clean-test:
	-rm -f specfun.test.exe
clean: clean-dev clean-test
	-rm -f libspecfun$(SHARED_LIB_EXT)


# Makefile debugging trick:
# call print-VARIABLE to see the runtime value of any variable
# (hardened against any special characters appearing in the output)
print-%:
	@echo '$*=$(subst ','\'',$(subst $(newline),\n,$($*)))'


.PHONY: clean clean-dev clean-test
