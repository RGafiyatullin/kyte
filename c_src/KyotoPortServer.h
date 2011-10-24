#ifndef _KyotoPortServer_h
#define _KyotoPortServer_h

#include <kcpolydb.h>

#include <PortServer.h>

#include <TaskQueue.h>

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
	opt_ThreadPoolSize = 1,
	opt_DebugLogFile = 2
};

class KPSResponse;

class KyotoPortServer : public PortServer {
private:
	static KyotoPortServer* Instance();

	KPSOptions_t _Options;
	const KPSOptions_t& Options() const;

	int ReadKPSOptions();

	RG::TaskQueue* _RequestQueue;
	RG::TaskQueue* _ResponseQueue;

	int InitThreadPool();
	int RecvLoop();
public:
	KyotoPortServer();
	virtual ~KyotoPortServer();

	static int GetPduType(const byte* packet, int packet_len);
	static int GetCommandId(const byte* packet, int packet_len);
	int Run();

	void Respond(KPSResponse * responseTask);
};

#endif // _KyotoPortServer_h
