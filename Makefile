# anchor

# --noinform is a runtime option. It must come before toplevel options.
SBCL := sbcl
LISP := $(SBCL) --noinform --non-interactive

.PHONY: all build bin test repl clean

all: test

build:
	$(LISP) --eval '(asdf:load-system :anchor)'

# Dump an executable into the project root with save-lisp-and-die.
bin:
	$(LISP) --eval '(asdf:make :anchor)'

test:
	$(LISP) --eval '(asdf:test-system :anchor)'

repl:
	$(SBCL) --eval '(asdf:load-system :anchor)'

# ASDF writes compiled output outside the tree. Only remove that cache.
clean:
	rm -rf $$HOME/.config/cache/common-lisp/*/$(CURDIR)
