# eggpd/Makefile
TARGETS=eggpd_app.beam \
	eggpd_super.beam \
	fibp.beam \
	ribp.beam \
	peerp.beam \
	peercp.beam \
	routemon.beam

.SUFFIXES: .erl .beam

.erl.beam:
	erlc +debug_info $<

all:  $(TARGETS)

clean:
	rm -f *.beam
