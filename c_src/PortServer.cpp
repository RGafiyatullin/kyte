

#include <PortServer.h>

#include <kps.h>

#include <stdlib.h>
#include <assert.h>

void check_io_failure() {
	int err = errno;
	if (err) {
		fprintf(dbgout, "\rKPS: io-failure - errno=%d\n", err);
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
	bool got_a_nil_packet = false;
	do {
		byte* packet = NULL;
		int packet_len = recv_packet(&packet);
		assert(packet_len != -1);
		if (packet_len == 0)
			got_a_nil_packet = true;
		
		free_packet(packet);
		packet = NULL;
	} while (!got_a_nil_packet);
}

void PortServer::send_packet(const byte* packet, int len) {
	uint32_t netLongPL = htonl(len);
	byte* pl_bytes = (byte*) &netLongPL;

	port_write_exact(pl_bytes, ERL_PACKET_LENGTH);
	port_write_exact(packet, len);
}
int  PortServer::recv_packet(byte** pPacket) {
	if ( *pPacket ) { // do not want to leak this buffer
		return -1;
	}

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



