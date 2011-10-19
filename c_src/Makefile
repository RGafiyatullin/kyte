KYOTO_I = /opt/kyoto/include
KYOTO_A = /opt/kyoto/lib/libkyotocabinet.a

ERLC = erlc
ASN1C = asn1c
CXX = g++ -c
LD = g++
CXX_FLAGS = -I$(KYOTO_I) -I../include
LD_FLAGS = -lz
ASN1SRCS = 
OBJECTS = KyotoPortServer.o PortServer.o main.o
BEAMS = t.beam kyoto_port_srv.beam
OUT_FILE = kyoto-port-server
RM_F = rm -f

all: all-cpp all-erl

all-cpp: $(OUT_FILE)

$(OUT_FILE): $(OBJECTS)
	$(LD) -o $(OUT_FILE) $(LD_FLAGS) $(OBJECTS) $(KYOTO_A)

all-erl: $(BEAMS)

clean:
	$(RM_F) $(OBJECTS)
	$(RM_F) $(OUT_FILE) 
	$(RM_F) $(BEAMS)

PortServer.o: PortServer.cpp PortServer.h
	$(CXX) $(CXX_FLAGS) PortServer.cpp

KyotoPortServer.o: PortServer.o KyotoPortServer.cpp KyotoPortServer.h
	$(CXX) $(CXX_FLAGS) KyotoPortServer.cpp

main.o: KyotoPortServer.h main.cpp
	$(CXX) $(CXX_FLAGS) main.cpp

kyoto_port_srv.beam: ../src/kyoto_port_srv.erl
	$(ERLC) -o . ../src/kyoto_port_srv.erl

t.beam: ../src/t.erl
	$(ERLC) -o . ../src/t.erl
