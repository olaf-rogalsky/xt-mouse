DEST=$(shell ls -td /home/olaf/local/share/emacs/[0-9]*/lisp | head -1)


install: xt-mouse.el.gz xt-mouse.elc
	install -m 644 $+ ${DEST}

diff: xt-mouse.el mouse.el
	mkdir -p orig new
	for i in $+; do \
	  cp /home/olaf/src/emacs/emacs/lisp/$$i orig; \
	  cp $$i new; \
	  diff -E -c orig/$$i new/$$i >$${i%.*}.patch; \
	done; \
	rm -rf orig new

push:
	git push origin master

xt-mouse.el.gz: xt-mouse.el
	gzip --keep $<

xt-mouse.elc: xt-mouse.el
	emacs -Q -batch --eval '(byte-compile-file "$<")'

