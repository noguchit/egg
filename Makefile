EMACS := emacs -q --no-site-file
SRCS  := egg-custom.el egg-base.el egg-const.el egg-git.el egg.el egg-grep.el egg-key.el
ELCS  := $(patsubst %.el,%.elc,$(SRCS))
DEPS  := $(patsubst %.el,.%.d,$(SRCS))

.PHONY: all

%.elc : %.el
	$(EMACS) -L . -batch -f batch-byte-compile $<

.%.d : %.el
	@echo Generating dependencies for $<
	@echo "$@ : $< " > $@
	@echo -n "$*.elc : $< " >> $@
	@sed -ne "s/^(require '\(egg.*\))/\1.elc/p" $< | tr '\n' ' ' >> $@
	@echo "" >> $@

all : $(DEPS) $(ELCS)

clean :
	-rm $(ELCS) $(DEPS)

-include $(DEPS)
