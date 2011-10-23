KYOTO_I = /opt/kyoto/include
KYOTO_A = /opt/kyoto/lib/libkyotocabinet.a
KPSASN_A = ../asn1/kpsasn1.a

ERLC = erlc
ASN1C = asn1c
CXX = g++ -c
LD = g++
CXX_FLAGS = -I. -I$(KYOTO_I) -I../asn1
LD_FLAGS = -lz
ASN1SRCS = 
OBJECTS = KPSTask.o Worker.o TaskQueue.o Monitor.o Thread.o ITask.o Mutex.o KyotoPortServer.o PortServer.o

OUT_FILE = kyoto-port-server
RM_F = rm -f

all: $(OUT_FILE)

$(OUT_FILE): $(OBJECTS) $(KPSASN_A) $(KYOTO_A) main.o
	$(LD) -o $(OUT_FILE) $(LD_FLAGS) $(OBJECTS) $(KPSASN_A) $(KYOTO_A) main.o

$(KPSASN_A):
	${MAKE} -C ../asn1

port-clean:
	${MAKE} -C ../asn1 clean
	$(RM_F) $(OBJECTS)
	$(RM_F) main.o
	$(RM_F) test.o
	$(RM_F) test


clean:
	${MAKE} -C ../asn1 clean
	$(RM_F) $(OBJECTS)
	$(RM_F) main.o
	$(RM_F) $(OUT_FILE)
	$(RM_F) test.o
	$(RM_F) test

KPSTask.o: KPSTask.cpp KPSTask.h ITask.h $(KPSASN_A)
	$(CXX) $(CXX_FLAGS) KPSTask.cpp

Worker.o: Worker.cpp Worker.h TaskQueue.h
	$(CXX) $(CXX_FLAGS) Worker.cpp

TaskQueue.o: TaskQueue.cpp TaskQueue.h ITask.h Worker.h Thread.h
	$(CXX) $(CXX_FLAGS) TaskQueue.cpp

Monitor.o: Mutex.h Monitor.h Monitor.cpp
	$(CXX) $(CXX_FLAGS) Monitor.cpp

Thread.o: Thread.cpp Thread.h ITask.h
	$(CXX) $(CXX_FLAGS) Thread.cpp

ITask.o: ITask.cpp ITask.h
	$(CXX) $(CXX_FLAGS) ITask.cpp

Mutex.o: Mutex.cpp Mutex.h
	$(CXX) $(CXX_FLAGS) Mutex.cpp

PortServer.o: PortServer.cpp PortServer.h
	$(CXX) $(CXX_FLAGS) PortServer.cpp

KyotoPortServer.o: $(KPSASN_A) PortServer.o KyotoPortServer.cpp KyotoPortServer.h
	$(CXX) $(CXX_FLAGS) KyotoPortServer.cpp

main.o: KyotoPortServer.h main.cpp
	$(CXX) $(CXX_FLAGS) main.cpp

test.o: test.cpp
	$(CXX) $(CXX_FLAGS) test.cpp

test: test.o $(OBJECTS) $(KYOTO_A) $(KPSASN_A)
	$(LD) -o test $(LD_FLAGS) $(OBJECTS) $(KPSASN_A) $(KYOTO_A) test.o
