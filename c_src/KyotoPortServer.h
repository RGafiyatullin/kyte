#ifndef _KyotoPortServer_h
#define _KyotoPortServer_h

#include <kcpolydb.h>

#include "PortServer.h"

using kyotocabinet::PolyDB;

/**
* 
* Port server communicates with erlang using the PDUs described in KyotoPS.asn1 file.
* 
* * * Encapsulation levels * * *
* Level-1 - Port-Server Packet: 4bytes - packet length, N-bytes packet-payload
* Level-2 - KyotoPS PDU: 2bytes - PDU-type, Rest - BER-encoded PDU
* Level-3 - BER-encoded PDU itself 
* 
*/

typedef struct KPSOptions_s {
	long ThreadPoolSize;
} KPSOptions_t;

enum KPSOptions {
	opt_EndOfOptions = 0,
	opt_ThreadPoolSize = 1
};

class KyotoPortServer : public PortServer {
private:
	static KyotoPortServer* Instance();

	KPSOptions_t _Options;
	const KPSOptions_t& Options() const;

	static int GetPduType(const byte* packet, int packet_len);
	int ReadKPSOptions();
public:
	KyotoPortServer();
	virtual ~KyotoPortServer();

	int run();


};

#endif // _KyotoPortServer_h
