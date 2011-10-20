#include <KPSSetOptionRequest.h>

#include "KyotoPortServer.h"

using kyotocabinet::PolyDB;

static KyotoPortServer* _Self = NULL;

KyotoPortServer::KyotoPortServer() : PortServer() {
	_Self = this;
}
KyotoPortServer::~KyotoPortServer() {}

KyotoPortServer* KyotoPortServer::Instance() {
	return _Self;
}

const KPSOptions_t& KyotoPortServer::Options() const {
	return _Options;
}

int KyotoPortServer::GetPduType(const byte* packet, int packet_len) {
	if (packet_len < 2) {
		return -1;
	}
	byte l, h;
	l = packet[0];
	h = packet[1];
	int pdu_type = l + (h << 8);
	return pdu_type;
}

int KyotoPortServer::ReadKPSOptions() {
	bool keep_reading = true;
	do {
		byte* packet = NULL;
		int packet_len = recv_packet(&packet);
		fprintf(stderr, "\rKyotoPortServer::ReadKPSOptions() len = %d\n", packet_len);
		assert(packet_len != -1);
		
		int pdu_type = GetPduType(packet, packet_len);
		fprintf(stderr, "\rKyotoPortServer::ReadKPSOptions() pdu_type = %d\n", pdu_type);
		//assert(pdu_type != -1);
		assert(pdu_type == 2);

		KPSSetOptionRequest_t * setOptionReq = NULL;
		asn_dec_rval_t dr =	ber_decode(0, &asn_DEF_KPSSetOptionRequest, (void**)&setOptionReq, packet + 2, packet_len - 2);
		assert(dr.code == RC_OK);

		fprintf(stderr, "\rKyotoPortServer::ReadKPSOptions() successfully decoded SetOptionReq\n");

		long iOptCode = 0;
		asn_INTEGER2long(&setOptionReq->optCode, &iOptCode);

		switch (iOptCode) {
			case opt_ThreadPoolSize: {
				long iThrPoolSize;
				asn_INTEGER2long( setOptionReq->optValueInteger, &iThrPoolSize);
				_Options.ThreadPoolSize = iThrPoolSize;
				fprintf(stderr, "\rKyotoPortServer::ReadKPSOptions() ThreadPoolSize = %d\n", iThrPoolSize);
				break;
			}
			case opt_EndOfOptions:
				fprintf(stderr, "\rKyotoPortServer::ReadKPSOptions() end of options reached\n");
				keep_reading = false;
				break;
			default:
				fprintf(stderr, "\rKyotoPortServer::ReadKPSOptions() Unknown optCode: %d\n", iOptCode);
		}

		free_packet(packet);
		packet = NULL;
	} while (keep_reading);

	return 123; // DEBUG

	return 0;
}

int KyotoPortServer::run() {
	fprintf(stderr, "\rKyotoPortServer::start()\n");

	int readOpts = ReadKPSOptions();
	if (readOpts) {
		return readOpts;
	}

	return 0;
}



