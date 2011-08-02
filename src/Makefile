# eggpd/Makefile
TARGETS=eggpd_app.beam \
	eggpd_sup.beam \
	eggpd_peer_sup.beam \
	eggpd.beam \
	fibp.beam \
	ribp.beam \
	eggpd_peer.beam \
	eggpd_connection.beam \
	routemon.beam

.SUFFIXES: .erl .beam

.erl.beam:
	erlc +debug_info $<

all:  $(TARGETS)

clean:
	rm -f *.beam
