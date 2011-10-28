ERL_INCLUDE = /opt/otp-r14b04-x86_64-full-word/lib/erlang/usr/include
KYOTO_I = /opt/kyoto/include
KYOTO_A = /opt/kyoto/lib/libkyotocabinet.a

CXX = g++ -c
LD = g++
CXX_FLAGS = -I. -I$(KYOTO_I) -I$(ERL_INCLUDE) -fPIC -Wall
LD_FLAGS = -lz -fPIC -shared -lstdc++
OBJECTS = \
			Worker.o \
			TaskQueue.o \
			Monitor.o \
			Thread.o \
			ITask.o \
			Mutex.o \
			\
			kyoto_client.o

OUT_FILE = libkyoto.so
RM_F = rm -f

CXXF = $(CXX) $(CXX_FLAGS)

all: $(OUT_FILE)

$(OUT_FILE): $(OBJECTS) $(KYOTO_A)
	$(LD) -o $(OUT_FILE) $(LD_FLAGS) $(OBJECTS) $(KYOTO_A)

clean:
	$(RM_F) $(OBJECTS)
	$(RM_F) $(OUT_FILE)


################################################################################

kyoto_client.o: kyoto_client.cpp kyoto_client.h
	$(CXXF) kyoto_client.cpp



Worker.o: Worker.cpp Worker.h TaskQueue.h
	$(CXXF) Worker.cpp

TaskQueue.o: TaskQueue.cpp TaskQueue.h ITask.h Worker.h Thread.h
	$(CXXF) TaskQueue.cpp

Monitor.o: Mutex.h Monitor.h Monitor.cpp
	$(CXXF) Monitor.cpp

Thread.o: Thread.cpp Thread.h ITask.h
	$(CXXF) Thread.cpp

ITask.o: ITask.cpp ITask.h
	$(CXXF) ITask.cpp

Mutex.o: Mutex.cpp Mutex.h
	$(CXXF) Mutex.cpp
