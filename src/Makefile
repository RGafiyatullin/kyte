
ERLC = erlc
ERL_OPTS = 

RM_F = rm -f

ASN1SRC = ../c_src/KyotoPS.asn1
GENERATED = KyotoPS.beam KyotoPS.erl KyotoPS.asn1db KyotoPS.hrl
MODULES = kps_seq_srv.beam kps_pdu.beam kyoto_port_srv.beam t.beam

all: $(MODULES)

clean:
	$(RM_F) $(MODULES)
	$(RM_F) $(GENERATED)

$(GENERATED): $(ASN1SRC)
	$(ERLC) $(ASN1SRC)

kps_seq_srv.beam:
	$(ERLC) $(ERL_OPTS) kps_seq_srv.erl

kps_pdu.beam: $(GENERATED) kps_pdu.erl
	$(ERLC) $(ERL_OPTS) kps_pdu.erl

kyoto_port_srv.beam: $(GENERATED) kps_pdu.beam kps_seq_srv.beam kyoto_port_srv.erl
	$(ERLC) $(ERL_OPTS) kyoto_port_srv.erl

t.beam: kyoto_port_srv.beam t.erl
	$(ERLC) $(ERL_OPTS) t.erl

