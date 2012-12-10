EMACS := emacs -q --no-site-file
SRCS  := egg-custom.el egg-base.el egg-const.el egg-git.el egg.el \
	 egg-grep.el egg-key.el egg-diff.el egg-svn.el
ELCS  := $(patsubst %.el,%.elc,$(SRCS))
DEPS  := $(patsubst %.el,.%.d,$(SRCS))

LOAD_DEPS  := $(patsubst %.el,.%.deps,$(SRCS))

.PHONY: all doc doc.clean clean loaddeps

%.elc : %.el
	@echo Compiling $<
	@$(EMACS) -L . -batch -f batch-byte-compile $<

.%.d : %.el
	@echo Generating dependencies for $<
	@echo "$@ : $< " > $@
	@echo ".$*.deps : $< " >> $@
	@echo -n "$*.elc : $< " >> $@
	@sed -ne "s/^(require '\(egg.*\))/\1.elc/p" $< | tr '\n' ' ' >> $@
	@echo "" >> $@

.%.deps : %.el
	@echo Generating load dependencies for $<
	@sed -ne "s/^(require '\(egg.*\))/\1 "$*"/p" $< > $@

all : $(DEPS) $(ELCS) egg-reload.el

doc :
	$(MAKE) -C doc

doc.clean :
	$(MAKE) -C doc clean

clean : doc.clean
	-rm $(ELCS) $(DEPS) $(LOAD_DEPS) egg-reload.el

loaddeps : $(LOAD_DEPS)

egg-reload.el : Makefile $(LOAD_DEPS)
	@echo Generating $@
	@cat $(filter-out $<, $^) | tsort | sed -nre 's/^(.+)$$/\(load "\1"\)/p' > $@
	@echo "(defun egg-reload ()\n  (interactive)" >> $@
	@echo "  (load \"egg-reload\"))\n" >> $@

-include $(DEPS)
