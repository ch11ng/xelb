PROTO_PATH := ../xcb-proto/src

EMACS_BIN := emacs -Q

EXTENSIONS := bigreq composite damage dpms dri2 dri3 ge glx present randr \
record render res screensaver shape shm sync xc_misc xevie xf86dri \
xf86vidmode xfixes xinerama xinput xkb xprint xselinux xtest xvmc xv

EXT_LIBS = $(addprefix xcb-,$(addsuffix .el,$(EXTENSIONS)))
LIBS = xcb-xproto.el $(EXT_LIBS)

all: clean $(LIBS)

xcb-%.el: $(PROTO_PATH)/%.xml
	@echo -n "\n"Generating $@...
	@$(EMACS_BIN) --script ./el_client.el $< > $@

$(EXT_LIBS): xcb-xproto.el

xcb-composite.el: xcb-xfixes.el
xcb-damage.el: xcb-xfixes.el
xcb-present.el: xcb-randr.el xcb-xfixes.el xcb-sync.el
xcb-randr.el: xcb-render.el
xcb-xfixes.el: xcb-render.el xcb-shape.el
xcb-xinput.el: xcb-xfixes.el
xcb-xvmc.el: xcb-xv.el
xcb-xv.el: xcb-shm.el

.PHONY: clean

clean:
	@rm -vf $(LIBS)
