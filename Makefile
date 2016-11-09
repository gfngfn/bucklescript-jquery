NMPATH=$(HOME)

lib/js/jquery.js: lib/ocaml/jquery.ml
	$(NMPATH)/node_modules/bs-platform/bin/bsc.exe -I $(CURDIR)/lib/ocaml -c -bs-main $(CURDIR)/lib/ocaml/jquery.ml -bs-package-name bucklescript-jquery -bs-package-output lib/js

test1:
	mkdir -p dist_test1
	$(NMPATH)/node_modules/bs-platform/bin/bsc.exe -I $(CURDIR)/lib/ocaml -c -bs-main $(CURDIR)/lib/ocaml/test.ml -bs-package-name test1 -bs-package-output $(subst $(HOME),,$(CURDIR))/dist_test1
	$(NMPATH)/node_modules/webpack/bin/webpack.js -p dist_test1/test.js dist_test1/bundle.js

