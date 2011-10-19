

#include "PortServer.h"

#include <stdlib.h>

void check_io_failure() {
	int err = errno;
	if (err) {
		fprintf(stderr, "\rKPS: io-failure - errno=%d\r\n", err);
		exit(1);
	}
}

PortServer::PortServer() : 
	_Input(fileno(stdin)), 
	_Output(fileno(stdout))
	{}
PortServer::PortServer(int input, int output) :
	_Input(input),
	_Output(output)
	{}
PortServer::PortServer(FILE* input, FILE* output) :
	_Input(fileno(input)),
	_Output(fileno(output))
	{}

PortServer::~PortServer() {}

int PortServer::port_read_exact(byte* buff, int len) {
	ssize_t i, got=0 ;
	do {
		i = read( _Input, buff + got, len - got);
		check_io_failure();
		if (i <= 0 ) {
			return i;
		}
		got += i;
	} while (got < len);
	return len;
}
int PortServer::port_write_exact(const byte* buff, int len) {
	int i; int wrote = 0 ;
	do {
		if ( ( i = write( _Output, buff + wrote, len - wrote)) <= 0  ) 
			return i;
		wrote += i;
	} while (wrote < len);
	return len;
}


void PortServer::init() {
	perror("\rKPS: PortServer::init() enter\r\n");
	bool got_a_nil_packet = false;
	do {
		byte* packet = NULL;
		int packet_len = recv_packet(&packet);

		fprintf(stderr, "\r KPS: PortServer:init() : got %d bytes \r\n", packet_len);

		if (packet_len == 0)
			got_a_nil_packet = true;
	} while (!got_a_nil_packet);
	perror("\rKPS: PortServer::init() leave\r\n");
}

void PortServer::send_packet(const byte* packet, int len) {
	uint32_t netLongPL = htonl(len);
	byte* pl_bytes = (byte*) &netLongPL;

	port_write_exact(pl_bytes, ERL_PACKET_LENGTH);
	port_write_exact(packet, len);
}
int  PortServer::recv_packet(byte** pPacket) {
	byte pl_bytes[ERL_PACKET_LENGTH];
	port_read_exact(pl_bytes, ERL_PACKET_LENGTH);
	
	uint32_t netLongPL  = * ((uint32_t *)pl_bytes);
	uint32_t hostLongPL = ntohl(netLongPL);

	byte* packet_buff = new byte[hostLongPL];
	port_read_exact(packet_buff, hostLongPL);

	*pPacket = packet_buff;
	return hostLongPL;
}
void PortServer::free_packet(byte* packet) {
	delete [] packet;
}



