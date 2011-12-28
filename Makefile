# Location of ocaml-dSFMT package
DPATH = ../ocaml-dSFMT
OD  = obj

all: ray.exe

# fetch files needed for ocaml-dSFMT random number package
$(OD)/libdsfmt.a: $(DPATH)/libdsfmt.a
	cp $(DPATH)/libdsfmt.a $(OD)
$(OD)/dsfmt.cmi:  $(DPATH)/dsfmt.cmi
	cp $(DPATH)/dsfmt.cmi $(OD)
$(OD)/dsfmt.o:    $(DPATH)/dsfmt.o
	cp $(DPATH)/dsfmt.o $(OD)
$(OD)/dsfmt.cmx:  $(DPATH)/dsfmt.cmx
	cp $(DPATH)/dsfmt.cmx $(OD)

ray.exe: $(OD)/libdsfmt.a $(OD)/dsfmt.cmi $(OD)/dsfmt.o $(OD)/dsfmt.cmx ray.ml
	cp ray.ml $(OD) ; cd $(OD) ; \
	ocamlopt -o $@ -verbose -inline 10 libdsfmt.a dsfmt.cmx ray.ml ; \
	mv $@ ..

# other possible optimization options:
# ocamlopt -linkall -nodynlink -unsafe -noassert

clean:
	rm ray.exe $(OD)/ray.cmi $(OD)/ray.o $(OD)/ray.cmx
