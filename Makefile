# Location of ocaml-dSFMT package

dSFMT-PATH = ocaml-dSFMT
OBJDIR  = obj
OPT = -inline 10 -I +gsl bigarray.cmxa gsl.cmxa -I .

all: exe

#$(dSFMT-PATH):
#	git clone git://github.com/samposm/ocaml-dSFMT.git ocaml-dSFMT

$(dSFMT-PATH)/libdsfmt.a $(dSFMT-PATH)/dsfmt.cmi $(dSFMT-PATH)/dsfmt.o $(dSFMT-PATH)/dsfmt.cmx: $(dSFMT-PATH)
	cd $(dSFMT-PATH) && make

# fetch files needed for ocaml-dSFMT random number package
$(OBJDIR)/libdsfmt.a: $(dSFMT-PATH)/libdsfmt.a
	cp $(dSFMT-PATH)/libdsfmt.a $(OBJDIR)
$(OBJDIR)/dsfmt.cmi:  $(dSFMT-PATH)/dsfmt.cmi
	cp $(dSFMT-PATH)/dsfmt.cmi $(OBJDIR)
$(OBJDIR)/dsfmt.a:    $(dSFMT-PATH)/dsfmt.a
	cp $(dSFMT-PATH)/dsfmt.a $(OBJDIR)
$(OBJDIR)/dsfmt.cmx:  $(dSFMT-PATH)/dsfmt.cmx
	cp $(dSFMT-PATH)/dsfmt.cmx $(OBJDIR)
$(OBJDIR)/dsfmt.cmxa:  $(dSFMT-PATH)/dsfmt.cmxa
	cp $(dSFMT-PATH)/dsfmt.cmxa $(OBJDIR)

exe: $(OBJDIR)/libdsfmt.a $(OBJDIR)/dsfmt.cmi $(OBJDIR)/dsfmt.a $(OBJDIR)/dsfmt.cmx $(OBJDIR)/dsfmt.cmxa ray.ml
	cp ray.ml $(OBJDIR) ; cd $(OBJDIR) ; \
ocamlopt -o $@ $(OPT) dsfmt.cmxa ray.ml ; \
mv $@ ..

# other possible optimization options:
# ocamlopt -linkall -nodynlink -unsafe -noassert

clean:
	rm exe $(OBJDIR)/ray.cmi $(OBJDIR)/ray.o $(OBJDIR)/ray.cmx
