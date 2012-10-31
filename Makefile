EMACS := emacs -q --no-site-file
SRCS  := egg-custom.el egg-base.el egg-const.el egg-git.el egg.el egg-grep.el egg-key.el
ELCS  := $(patsubst %.el,%.elc,$(SRCS))
DEPS  := $(patsubst %.el,.%.d,$(SRCS))

.PHONY: all doc doc.clean clean

%.elc : %.el
	@echo Compiling $<
	@$(EMACS) -L . -batch -f batch-byte-compile $<

.%.d : %.el
	@echo Generating dependencies for $<
	@echo "$@ : $< " > $@
	@echo -n "$*.elc : $< " >> $@
	@sed -ne "s/^(require '\(egg.*\))/\1.elc/p" $< | tr '\n' ' ' >> $@
	@echo "" >> $@

all : $(DEPS) $(ELCS)

doc :
	$(MAKE) -C doc

doc.clean :
	$(MAKE) -C doc clean

clean : doc.clean
	-rm $(ELCS) $(DEPS)

-include $(DEPS)
