#include "KyotoPortServer.h"

using kyotocabinet::PolyDB;

KyotoPortServer::KyotoPortServer() : PortServer(), _State(st_negotiating) {}
KyotoPortServer::~KyotoPortServer() {}

void KyotoPortServer::start() {
	bool keep_running = true;
	do {
		fprintf(stderr, "\rKPS: state - %d\r\n", _State);
		switch (_State) {
			case st_negotiating:
				state_negotiating();
			break;
			case st_open:
				state_open();
			break;
			default:
				keep_running = false;
			break;
		}
	} while (keep_running);
}

void KyotoPortServer::state_negotiating() {
	perror("\rKyotoPortServer:state_negotiating() enter\r\n");
	bool got_a_nil_packet = false;
	do {
		byte* packet = NULL;
		int packet_len = recv_packet(&packet);
		fprintf(stderr, "\r KPS: PortServer:state_negotiating() : got %d bytes \r\n", packet_len);
		
		if (packet_len > 0) {
			byte opt_code = packet[0];
			
			switch (opt_code) {
				case noc_db_file: {
					int str_len = packet_len - 1;
					for (int i = 0; i < str_len; i++) {
						_DBFile[i] = packet[i + 1];
					}
					_DBFile[str_len] = '\0';
					fprintf(stderr, "\r KPS: PortServer:state_negotiating() : db-file: '%s'\r\n", _DBFile);
				}
				break;
				default:
					fprintf(stderr, "\r KPS: PortServer:state_negotiating() : got unknown command #%d\r\n", opt_code);
				break;
			}
		}
		else {
			got_a_nil_packet = true;
		}
		free_packet(packet);
	} while (!got_a_nil_packet);
	
	if (_DB.open( _DBFile, PolyDB::OWRITER | PolyDB::OCREATE )) {
		fprintf(stderr, "\rKPS: PolyDB openned successfully\r\n");
		byte reply_packet[] = "open";
		send_packet(reply_packet, sizeof(reply_packet) - 1);

		_State = st_open;
	}
	else {
		fprintf(stderr, "\rKPS: PolyDB failed to open: %s\r\n", _DB.error().name());
		byte reply_packet[] = "fail";
		send_packet(reply_packet, sizeof(reply_packet) - 1);

		_State = st_unmanaged;
	}

	perror("\rKyotoPortServer:state_negotiating() leave\r\n");
}

void KyotoPortServer::state_open() {
	perror("\rKyotoPortServer:state_open() enter\r\n");
	bool got_a_nil_packet = false;
	do {
		byte* packet = NULL;
		int packet_len = recv_packet(&packet);
		fprintf(stderr, "\r KPS: PortServer:state_open() : got %d bytes \r\n", packet_len);
		
		if (packet_len > 0) {
			byte opt_code = packet[0];
			
			switch (opt_code) {
				default:
					fprintf(stderr, "\r KPS: PortServer:state_open() : got unknown command #%d\r\n", opt_code);
				break;
			}
		}
		else {
			got_a_nil_packet = true;
		}
		free_packet(packet);
	} while (!got_a_nil_packet);

	_DB.close();
	_State = st_unmanaged;
	perror("\rKyotoPortServer:state_open() leave\r\n");
}

