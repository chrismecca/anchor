# anchor

# cl-adoc is not in the ocicl registry yet, so it is resolved from a local
# checkout. Override CL_ADOC when yours is somewhere else. This drops out
# once the registry submission lands.
CL_ADOC ?= $(CURDIR)/../cl-adoc
export CL_SOURCE_REGISTRY = (:source-registry (:directory "$(CL_ADOC)") :inherit-configuration)

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
