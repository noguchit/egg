EMACS := emacs -q --no-site-file
SRCS  := egg-custom.el egg-base.el egg-const.el egg.el egg-grep.el egg-key.el
ELCS  := $(patsubst %.el,%.elc,$(SRCS))

.PHONY: all foo

%.elc : %.el
	$(EMACS) -L . -batch -f batch-byte-compile $<

all : $(ELCS)

clean :
	-rm $(ELCS)

foo :
	echo $(ELCS)

egg-custom.elc : egg-custom.el
egg-base.elc : egg-base.el egg-custom.el
egg-const.elc : egg-const.el egg-custom.el
egg.elc : egg.el egg-custom.el egg-base.el
egg-grep.elc : egg-grep.el egg.el egg-base.el egg-custom.el
egg-key.elc : egg-key.el egg.el egg-base.el egg-custom.el