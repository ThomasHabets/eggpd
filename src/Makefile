# eggpd/Makefile
TARGETS=eggpd_app.beam \
	eggpd_sup.beam \
	eggpd_peer_sup.beam \
	eggpd.beam \
	eggpd_fibp.beam \
	eggpd_ribp.beam \
	eggpd_peer.beam \
	eggpd_connection.beam \
	routemon.beam

.SUFFIXES: .erl .beam

.erl.beam:
	erlc $<

all:  $(TARGETS)

clean:
	rm -f *.beam
