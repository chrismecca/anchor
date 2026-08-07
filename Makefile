# anchor

# --noinform is a runtime option, so it has to precede the toplevel options.
SBCL := sbcl
LISP := $(SBCL) --noinform --non-interactive

.PHONY: all build bin test repl clean

all: test

build:
	$(LISP) --eval '(asdf:load-system :anchor)'

# Dumps an executable into the project root via save-lisp-and-die.
bin:
	$(LISP) --eval '(asdf:make :anchor)'

test:
	$(LISP) --eval '(asdf:test-system :anchor)'

repl:
	$(SBCL) --eval '(asdf:load-system :anchor)'

# ASDF caches compiled output outside the tree, so there is nothing to remove
# here beyond that cache.
clean:
	rm -rf $$HOME/.config/cache/common-lisp/*/$(CURDIR)
