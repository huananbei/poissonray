CC   = gcc

# from dSFMT-src-2.2/Makefile, with slight editing:
WARN = -Wmissing-prototypes -W -Winline 
OPTI = -O3 -finline-functions -fomit-frame-pointer -DNDEBUG \
       -fno-strict-aliasing --param max-inline-insns-single=1800 
STD  = -std=c99
CCFLAGS = $(OPTI) $(WARN) $(STD)
CCFLAGS += --param inline-unit-growth=500 --param large-function-growth=900
SSE2FLAGS = -msse2 -DHAVE_SSE2

# These I added:
MFLAG = -DDSFMT_MEXP=19937
ARCHFLAG = -march=native
CCFLAGS += $(STD) $(SSE2FLAGS) $(MFLAG) $(ARCHFLAG)

dSFMTDIR = ../dSFMT-src-2.2

all: test_c1 test_c2 test_ocaml test_ocaml_random

../dSFMT_c.o: 
	cd .. ; make

test_c1: test_c1.c ../dSFMT_c.o
	$(CC) $(CCFLAGS) -I$(dSFMTDIR) -o $@ $^

test_c2: test_c2.c ../dSFMT_c.o
	$(CC) $(CCFLAGS) -I$(dSFMTDIR) -o $@ $^

../libdsfmt.a:
	cd .. ; make

../dsfmt.cmx:
	cd .. ; make

test_ocaml: ../libdsfmt.a ../dsfmt.cmx test_ocaml.ml
	ocamlopt -inline 2 -I .. $^ -o $@

test_ocaml_random: test_ocaml_random.ml
	ocamlopt -inline 2 $< -o $@

clean:
	rm test_c1 test_c2 test_ocaml test_ocaml.cmi test_ocaml.o \
           test_ocaml.cmx test_ocaml_random test_ocaml_random.cmi \
           test_ocaml_random.o test_ocaml_random.cmx
