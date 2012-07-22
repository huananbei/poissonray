OCAMLPATH = `ocamlc -where`

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

dSFMTDIR = dSFMT-src-2.2

all: dsfmt.cmi dsfmt.cmo dsfmt.cma dlldsfmt.so \
     dsfmt.o dsfmt.cmx dsfmt.a dsfmt.cmxa libdsfmt.a

dSFMT_c.o: $(dSFMTDIR)/dSFMT.c $(dSFMTDIR)/dSFMT.h \
           $(dSFMTDIR)/dSFMT-params.h $(dSFMTDIR)/dSFMT-params19937.h 
	$(CC) $(CCFLAGS) -c $< -o $@

dsfmt-wrap.o: dsfmt-wrap.c $(dSFMTDIR)/dSFMT.h
	$(CC) $(CCFLAGS) -I $(OCAMLPATH) -I$(dSFMTDIR) -c $<

dsfmt.cmi dlldsfmt.so dsfmt.cmo dsfmt.cma libdsfmt.a dsfmt.o dsfmt.cmx \
dsfmt.a dsfmt.cmxa: dsfmt.ml dsfmt-wrap.o dSFMT_c.o
	ocamlmklib -v -ocamlopt "ocamlopt -inline 2" -o dsfmt $^

# other possible optimization options:
# ocamlopt -linkall -nodynlink -unsafe -noassert

clean:
	rm dsfmt-wrap.o dSFMT_c.o \
           dsfmt.cmi dsfmt.cmo dsfmt.cma dlldsfmt.so \
           dsfmt.o dsfmt.cmx dsfmt.a dsfmt.cmxa libdsfmt.a
