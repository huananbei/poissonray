# Location of ocaml-dSFMT package

dSFMT-PATH = ocaml-dSFMT
OBJDIR  = obj
OPT = -inline 10

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
$(OBJDIR)/dsfmt.o:    $(dSFMT-PATH)/dsfmt.o
	cp $(dSFMT-PATH)/dsfmt.o $(OBJDIR)
$(OBJDIR)/dsfmt.cmx:  $(dSFMT-PATH)/dsfmt.cmx
	cp $(dSFMT-PATH)/dsfmt.cmx $(OBJDIR)

exe: $(OBJDIR)/libdsfmt.a $(OBJDIR)/dsfmt.cmi $(OBJDIR)/dsfmt.o $(OBJDIR)/dsfmt.cmx ray.ml
	cp ray.ml $(OBJDIR) ; cd $(OBJDIR) ; \
ocamlopt -o $@ $(OPT) libdsfmt.a dsfmt.cmx ray.ml ; \
mv $@ ..

# other possible optimization options:
# ocamlopt -linkall -nodynlink -unsafe -noassert

clean:
	rm exe $(OBJDIR)/ray.cmi $(OBJDIR)/ray.o $(OBJDIR)/ray.cmx
