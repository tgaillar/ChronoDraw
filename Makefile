all: svg

TMG_FILE_STEM = %.tmg
SVG_FILE_STEM = %.svg
EMF_FILE_STEM = %.emf

TMG_FILES = \
  TEST/RenderTMG.tmg \
  TEST/clock.tmg \
  \
  TEST/TextWidth.tmg \
  \
  TEST/SignalAlignment.tmg \
  TEST/TimingAlignment.tmg \
  \
  TEST/SignalTransition1.tmg \
  TEST/SignalTransition2.tmg \
  \
  TEST/ClockTransition1.tmg \
  TEST/ClockTransition2.tmg \
  \
  TEST/TimingReference.tmg \
  \
  TEST/CLOCK-constraints.tmg \
  \
  TEST/SignalJitter.tmg \
#

clean::
	$(rm) TEST/core TEST/*~

BASE_LIST := $(patsubst $(TMG_FILE_STEM),%,$(TMG_FILES))

SVG_FILES := $(patsubst %,$(SVG_FILE_STEM),$(BASE_LIST))
EMF_FILES := $(patsubst %,$(EMF_FILE_STEM),$(BASE_LIST))

# Reference the timing to SVG converter
chronodraw := chronodraw.pl
CHRONODRAW := ./$(firstword $(chronodraw))
ifdef DSVG
  chronodraw := $(chronodraw) -dsvg=$(DSVG)
endif

.PHONY: svg
svg: $(SVG_FILES)

$(SVG_FILES): $(SVG_FILE_STEM) : $(TMG_FILE_STEM) Makefile $(CHRONODRAW)
	@$(target-build-header); \
	 $(target-build-message) "Deriving SVG from TMG ($(dependency-first))"
	@$(chronodraw) $(dependency-first) > $(tmp-target) 2>$(target-log); \
	 $(save-exit-code); \
	 $(output-target-log); \
	 $(rm) $(target-log); \
	 $(exit-with-saved-code)
	@$(chmod) a-w $(tmp-target); \
	 $(mv) $(tmp-target) $(target)

realclean::
	$(rm) $(SVG_FILES)


.PHONY: emf
emf: $(EMF_FILES)

$(EMF_FILES): $(EMF_FILE_STEM) : $(SVG_FILE_STEM) Makefile
	@$(target-build-header); \
	 $(target-build-message) "Deriving EMF from SVG ($(dependency-first))"
	@tmpsvg=".tmpsvg$$$$"; \
	 $(rm) $${tmpsvg}; \
	 $(cp) $(dependency-first) $${tmpsvg}; \
	 svg2emf $${tmpsvg} >$(target-log) 2>&1; \
	 $(save-exit-code); \
	 $(output-target-log); \
	 $(rm) $(target-log); \
	 $(rm) $${tmpsvg}; \
	 $(mv) $${tmpsvg}.emf $(tmp-target); \
	 $(exit-with-saved-code)
	@$(chmod) a-w $(tmp-target); \
	 $(mv) $(tmp-target) $(target)

realclean::
	$(rm) $(EMF_FILES)



.PHONY: clean
clean::
	$(rm) core *~

.PHONY: realclean
realclean:: clean


#
# Commonly used variables and macros
#

ifndef __MAKE_CODE_DIR__
  __MAKE_CODE_DIR__ = MAKE
endif

ifndef __MAKE_MACRO__
  __MAKE_MACRO__ = $(__MAKE_CODE_DIR__)/MACRO.mk
  __MAKEFILE__  += $(__MAKE_MACRO__)

# Explicit or inaccessible characters
# (_EMPTY_ intentionally left undefined...)
_COLON_  := :
_COMMA_  := ,
_SPACE_  := $(_EMPTY_) $(_EMPTY_)
_DOLLAR_ := $$
_BSLASH_ := $(_EMPTY_)\$(_EMPTY_)
_LANGLE_ := <
_RANGLE_ := >
_QUOTE_  := '
_BQUOTE_ := `
_DQUOTE_ := "
# Dummy comment to overcome Emacs font-lock problem... "`'

# Standard Unix commands
cat	= cat /dev/null
echo    = echo
touch	= touch -f
rm	= /bin/rm -f
mv	= /bin/mv -f
ln	= /bin/ln -s
cp	= /bin/cp
mkdir	= /bin/mkdir -p
rmdir	= /bin/rm -rf
chmod	= /bin/chmod
sed     = /bin/sed

# Usefull (semi-complex) Unix commands
downcase   = tr '[A-Z]' '[a-z]'
upcase     = tr '[a-z]' '[A-Z]'
under2dash = tr '_' '-'
dash2under = tr '-' '_'
space2newline = tr ' ' '\012'
newline2space = tr '\012' ' '
message  = echo 1>&2
echoindent = $(echo)

# Useful (?) make variables
override VOID :=
override NIL  :=
override NONE :=

# Debug wrapper
HELP_MODIFIERS += "$(HELPt)DEBUG: display debug messages."
ifdef DEBUG
  echodebug = echo
else
  echodebug = true
endif

# How to time commands
HELP_MODIFIERS += "$(HELPt)TIME: run \`time' in front of some commands"
ifdef TIME
  time-maybe := time
else
  time-maybe =
endif



tmp-file-stem = %.tmp
log-file-stem = %.log


#
# Make automatic variables shortcuts
#

target            = $(@)
tmp-target        = $(patsubst %,$(tmp-file-stem),$(@))
target-log        = $(patsubst %,$(log-file-stem),$(@))
target-file       = $(@F)
target-directory  = $(@D)

stem              = $(*)
stem-file         = $(*F)
stem-dir          = $(*D)

dependency-list   = $(^)
dependency-new    = $(?)
dependency-first  = $(<)


#
# Process-related commands/variables
#
saved-code               = $${exitcode}
clear-saved-code         = exitcode=0
set-saved-code           = exitcode=1
save-exit-code           = exitcode=$$?
add-status-to-saved-code = exitstatus=$$?; exitcode=`expr $${exitcode} + $${exitstatus}`
exit-with-saved-code     = exit $${exitcode}



#
# Output-related commands (debug/explain/text
#

target-message          = $(message) ""; $(message) 
target-build-header     = $(message) ""; $(message) "Building $(target)..."
target-build-message    = $(message) "  -"
target-build-submessage = $(message) "     "

target-build-message-proto    = for line in @MESSAGE@; do $(target-build-message) "$$line"; done
target-build-submessage-proto = for line in @MESSAGE@; do $(target-build-submessage) "$$line"; done


ifndef DEBUG
  debug-target-wrt-dependencies-maybe = true
else
  EXPLAIN = 1
  define debug-target-wrt-dependencies-maybe
    $(echo) ""; \
    $(echoindent) ">> Target       : " $(target); \
    $(echo)       ">> -------------- "; \
    $(echoindent) ">>   Stem       : " $(stem); \
    $(echoindent) ">>   Stem File  : " $(stem-file); \
    $(echoindent) ">>   Stem Dir   : " $(stem-dir); \
    $(echoindent) ">>   Reason     : " $(dependency-new); \
    $(echoindent) ">>   Dependency : " $(dependency-list)
  endef
endif

HELP_MODIFIERS += "$(HELPt)EXPLAIN: display information about target rebuild."
ifndef EXPLAIN
  explain-target-wrt-dependencies-maybe = $(target-build-header)
else
  define explain-target-wrt-dependencies-maybe
    $(echo) ""; \
    $(echoindent) ">> Target       : " $(target); \
    $(echo)       ">> -------------- "; \
    $(echoindent) ">>   Reason     : " $(notdir $(dependency-new)); \
    $(echoindent) ">>   Dependency : " $(notdir $(dependency-list))
  endef
endif

HELP_MODIFIERS += "$(HELPt)VERBOSE: make sub-commands be somewhat more verbose (valid values are ALL, ERROR, WARNING or <none>)." \
		  "$(HELPt)VERBOSE_REGEXP: specify an alternate regexp to parse sub-commands output (VERBOSE implied)." \
		  "$(HELPt)VERBOSE_COMMAND: specify an alternate command to parse sub-commands output (VERBOSE implied)."
output-with-indent = awk '{print "    | " $$0}'
output-subst-file  = $(output-with-indent) %
output-target-log  = $(output-with-indent) $(target-log)
output-tmp-target  = $(output-with-indent) $(tmp-target)
ifeq (,$(strip $(VERBOSE)$(VERBOSE_REGEXP)$(VERBOSE_COMMAND)))
  output-subst-file-maybe = true
  output-target-log-maybe = true
  output-tmp-target-maybe = true
  output-subst-file-wrt-saved-code-maybe = if [ $$exitcode -ne 0 ]; then $(output-subst-file); fi
  output-target-log-wrt-saved-code-maybe = if [ $$exitcode -ne 0 ]; then $(output-target-log); fi
  output-tmp-target-wrt-saved-code-maybe = if [ $$exitcode -ne 0 ]; then $(output-tmp-target); fi
else

  ifdef VERBOSE_COMMAND
   pipe-filter-output-wrt-verbose-maybe = | $(VERBOSE_COMMAND)
  else
    ifdef VERBOSE_REGEXP
     pipe-filter-output-wrt-verbose-maybe = | egrep -i '$(VERBOSE_REGEXP)'
    else
      ifeq (WARNING,$(VERBOSE))
       pipe-filter-output-wrt-verbose-maybe = | egrep -i '(warning|error)'
      else
        ifeq (ERROR,$(VERBOSE))
	  pipe-filter-output-wrt-verbose-maybe = | egrep -i '(error)'
        else
	  pipe-filter-output-wrt-verbose-maybe = 
        endif
      endif
    endif
  endif

  output-subst-file-maybe = $(output-subst-file) $(pipe-filter-output-wrt-verbose-maybe)
  output-target-log-maybe = $(output-target-log) $(pipe-filter-output-wrt-verbose-maybe)
  output-tmp-target-maybe = $(output-tmp-target) $(pipe-filter-output-wrt-verbose-maybe)
  output-subst-file-wrt-saved-code-maybe = if [ $$exitcode -ne 0 ]; then $(output-subst-file); fi $(pipe-filter-output-wrt-verbose-maybe)
  output-target-log-wrt-saved-code-maybe = if [ $$exitcode -ne 0 ]; then $(output-target-log); fi $(pipe-filter-output-wrt-verbose-maybe)
  output-tmp-target-wrt-saved-code-maybe = if [ $$exitcode -ne 0 ]; then $(output-tmp-target); fi $(pipe-filter-output-wrt-verbose-maybe)

endif



#
# Target-related commands/variables
#

create-target-directory = $(mkdir) $(target-directory)

# Update target last-modification time so MAKE will delete it
# whenever target cannot be completed
make-target-not-precious = $(rm) $(target) $(tmp-target) $(target-log)
make-target-not-writable = $(chmod) a-w $(target)

barf-because-target-not-found = $(message) "Error, file $(target) does not exist"; exit 2

remove-tmp-target      = $(rm) $(tmp-target)
update-target-from-tmp = $(rm) $(target); $(mv) $(tmp-target) $(target); $(make-target-not-writable)

define update-target-from-tmp-if-different-only
  if [ ! -f $(target) ]; then \
    $(echodebug) "Creating $(target)..."; \
    $(update-target-from-tmp); \
  elif diff $(tmp-target) $(target) > /dev/null 2>&1; then \
    $(echodebug) "$(target) is OK..."; \
    $(remove-tmp-target); \
  else \
    $(echodebug) "Recreating $(target)..."; \
    $(update-target-from-tmp); \
  fi
endef



#
# Automagic header related proto command/variables...
#
AUTOMAGIC_HEADER_LINES_PROTO = \
  @COMMENT@"" \
  @COMMENT@"" \
  @COMMENT@" WARNING, THIS FILE AUTOMAGICALLY GENERATED" \
  @COMMENT@"" \
  @COMMENT@" PLEASE DO NEVER MODIFY IT BY HAND" \
  @COMMENT@"" \
  @COMMENT@""

define echo-automagic-header-proto
  (for line in $(AUTOMAGIC_HEADER_LINES_PROTO); do \
     echo "$$line"; \
   done)
endef

define create-tmp-target-automagic-header-proto
  $(echo-automagic-header-proto) > $(tmp-target)
endef

#
# Automagic header related commands/variables
#

#AUTOMAGIC_HEADER_LINES = $(subst @COMMENT@,\#,$(AUTOMAGIC_HEADER_LINES_PROTO))
AUTOMAGIC_HEADER_LINES = \
  "\# WARNING, THIS FILE AUTOMAGICALLY GENERATED" \
  "\#" \
  "\# PLEASE DO NEVER MODIFY IT BY HAND" \
  "\#" \
  ""

define echo-automagic-header
  (for line in $(AUTOMAGIC_HEADER_LINES); do \
     echo "$$line"; \
   done)
endef

define create-tmp-target-automagic-header
  $(echo-automagic-header) > $(tmp-target)
endef

endif #__MAKE_MACRO__
