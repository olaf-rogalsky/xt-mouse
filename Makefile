diff: xt-mouse.el mouse.el
	mkdir -p orig new
	for i in $+; do \
	  cp /home/olaf/src/emacs/emacs/lisp/$$i orig; \
	  cp $$i new; \
	  diff -c orig/$$i new/$$i >$${i%.*}.patch; \
	done; \
	rm -rf orig new

push:
	git push origin master
