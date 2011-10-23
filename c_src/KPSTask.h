#ifndef _KPSTask_h
#define _KPSTask_h

#define ENC_BUFF_SIZE 65535

#include <stdio.h>
#include <assert.h>
#include <ITask.h>

// ls ../asn1/*.h | grep KPS | while read H; do echo "#include <$H>"; done
#include <KPSBasicRequest.h>
#include <KPSBasicResponse.h>
#include <KPSCommandId.h>
#include <KPSDbClearRequest.h>
#include <KPSDbClearResponse.h>
#include <KPSDbCloseRequest.h>
#include <KPSDbCloseResponse.h>
#include <KPSDbCountRequest.h>
#include <KPSDbCountResponse.h>
#include <KPSDbGetRequest.h>
#include <KPSDbGetResponse.h>
#include <KPSDbHandle.h>
#include <KPSDbOpenRequest.h>
#include <KPSDbOpenResponse.h>
#include <KPSDbRemoveRequest.h>
#include <KPSDbRemoveResponse.h>
#include <KPSDbSetRequest.h>
#include <KPSDbSetResponse.h>
#include <KPSDbSizeRequest.h>
#include <KPSDbSizeResponse.h>
#include <KPSSetOptionRequest.h>
#include <KPSSetOptionResponse.h>


typedef uint8_t byte;

class KyotoPortServer;

class KPSTask : public RG::ITask {
private:
	byte _CID[2];
	KyotoPortServer* _KPS;
	byte* _Packet;
	int _PacketLen;

public:
	KPSTask(KyotoPortServer* kps, byte* packet, int packet_len);
	virtual ~KPSTask();

	virtual void Run();
	virtual bool ToBeDisposedByWorker() const;


	void process_KPSBasicRequest(const KPSBasicRequest_t * req);
	void process_KPSSetOptionRequest(const KPSSetOptionRequest_t * req);
	void process_KPSDbOpenRequest(const KPSDbOpenRequest_t * req);
	void process_KPSDbCloseRequest(const KPSDbCloseRequest_t * req);
	void process_KPSDbClearRequest(const KPSDbClearRequest_t * req);
	void process_KPSDbCountRequest(const KPSDbCountRequest_t * req);
	void process_KPSDbSizeRequest(const KPSDbSizeRequest_t * req);
	void process_KPSDbSetRequest(const KPSDbSetRequest_t * req);
	void process_KPSDbGetRequest(const KPSDbGetRequest_t * req);
	void process_KPSDbRemoveRequest(const KPSDbRemoveRequest_t * req);

};

class KPSResponse : public RG::ITask {
private:
	KyotoPortServer* _KPS;
	byte* _Packet;
	int _PacketLen;
public:
	KPSResponse(KyotoPortServer* kps, byte* packet, int packet_len);
	virtual ~KPSResponse();

	virtual void Run();
	virtual bool ToBeDisposedByWorker() const;
};

#endif // _KPSTask_h
