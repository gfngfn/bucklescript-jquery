NMPATH=$(HOME)

lib/js/jquery.js: lib/ocaml/jquery.ml
	mkdir -p lib/js
	$(NMPATH)/node_modules/bs-platform/bin/bsc.exe -I $(CURDIR)/lib/ocaml -c -bs-main $(CURDIR)/lib/ocaml/jquery.ml -bs-package-name bucklescript-jquery -bs-package-output $(subst $(HOME),,$(CURDIR))/lib/js

copy:
	cp lib/ocaml/jquery.ml lib/ocaml/jquery.cmi lib/ocaml/jquery.cmj $(NMPATH)/node_modules/bs-platform/lib/ocaml/
	cp lib/js/jquery.js $(NMPATH)/node_modules/bs-platform/lib/js/

test1: test1/src/test.ml
	mkdir -p test1/dist
	$(NMPATH)/node_modules/bs-platform/bin/bsc.exe -c -bs-package-include bucklescript-jquery -bs-files $(CURDIR)/test1/src/test.ml
#-bs-package-name test1 -bs-package-output $(subst $(HOME),,$(CURDIR))/test1/dist
	$(NMPATH)/node_modules/webpack/bin/webpack.js -p test1/dist/test.js test1/dist/bundle.js

.PHONY: test1
