PROTO_PATH := ../xcb-proto/src

EMACS_BIN := emacs -Q

EXTENSIONS := bigreq dpms ge present render shape xf86dri xinerama xprint \
xtest composite dri2 glx randr res shm xc_misc xf86vidmode xinput xvmc \
damage dri3 record screensaver sync xevie xfixes xkb xselinux xv

LIBS = xcb-xproto.el $(addprefix xcb-,$(addsuffix .el,$(EXTENSIONS)))

all: clean $(LIBS)

xcb-%.el: $(PROTO_PATH)/%.xml xcb-xproto.el
	@echo -n "\n"Generating $@...
	@$(EMACS_BIN) --script ./el_client.el $< > $@

xcb-composite.el: xcb-xfixes.el
xcb-damage.el: xcb-xfixes.el
xcb-xvmc.el: xcb-xv.el
xcb-randr.el: xcb-render.el
xcb-xfixes.el: xcb-render.el xcb-shape.el
xcb-present.el: xcb-randr.el xcb-xfixes.el xcb-sync.el
xcb-xv.el: xcb-shm.el
xcb-xinput.el: xcb-xfixes.el

.PHONY: clean

clean:
	@rm -vf $(LIBS)
