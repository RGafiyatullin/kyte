
CXX = g++ -c
CXX_FLAGS = -I./generated
LD = g++
ASN1C = asn1c

MKDIR = mkdir -p
RM_F = rm -f

EXE = asnapi
OBJECTS = asnapi.o

all: $(EXE)
	echo "SUCCESS!"

clean:
	$(RM_F) $(OBJECTS)
	$(RM_F) $(EXE)
	(cd generated; ${MAKE} clean)

generated/generated.a: KyotoPS.asn1
	( cd generated ; ${MAKE} )

asnapi.o: generated/generated.a asnapi.cpp
	$(CXX) $(CXX_FLAGS) asnapi.cpp

$(EXE): generated/generated.a asnapi.o
	$(LD) -o $(EXE) asnapi.o generated/generated.a