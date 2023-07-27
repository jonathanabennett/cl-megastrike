##
# Megastrike
#
# Dependencies:
#  * SBCL
#  * Quicklisp
#  * An X-Window System
# @file
# @version 1.2.1

LISP ?= sbcl

build:
	$(LISP) --eval "(require 'asdf)" \
		--load megastrike.asd \
		--eval '(asdf:make :megastrike)' \
		--eval '(quit)'
		echo "The executable can be found in the bin/ directory."

# end
