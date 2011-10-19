
#ifndef _port_srv_routines_h
#define _port_srv_routines_h

#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <netinet/in.h>

#define ERL_PACKET_LENGTH 4

typedef unsigned char byte;

/**
* Protocol:
* There come the initialization packets
* After the initialization packets comes nil-packet
*/

class PortServer {
private:
	int _Input;
	int _Output;
	//byte _PacketSizeIndicatorLen;

	int port_read_exact(byte* buff, int len);
	int port_write_exact(const byte* buff, int len);
protected:

public:
	PortServer();
	PortServer(int input, int output);
	PortServer(FILE* input, FILE* output);
	virtual ~PortServer();

	virtual void init();

	void send_packet(const byte* packet, int len);
	int  recv_packet(byte** pPacket);
	void free_packet(byte* packet);
};

#endif // _port_srv_routines_h

