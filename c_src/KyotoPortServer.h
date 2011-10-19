#ifndef _KyotoPortServer_h
#define _KyotoPortServer_h

#include <kcpolydb.h>

#include "PortServer.h"

using kyotocabinet::PolyDB;

/**
* FSM states:
* 0. negotiating
* 1. open
* 2. closed
* 
* # State: negotiating
* When negotiating erlang side sends packets to the port-server.
* This packets' series is terminated with nil-packet.
* When nil-packet is got - server switches to the 'closed' state
* 
*/

enum KPS_State {
	st_negotiating,
	st_open,
	st_unmanaged
};

enum KPS_Nego_OptCode {
	noc_db_file = 0
};

class KyotoPortServer : public PortServer {
private:
	char _DBFile[256];
	PolyDB _DB;

	KPS_State _State;

	void state_negotiating();
	void state_open();
	void state_unmanaged();
public:
	KyotoPortServer();
	virtual ~KyotoPortServer();

	void start();
};

#endif // _KyotoPortServer_h
