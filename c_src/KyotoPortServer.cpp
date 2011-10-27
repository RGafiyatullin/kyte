#include <KPSSetOptionRequest.h>

#include <kps.h>

#include <KyotoPortServer.h>
#include <KPSTask.h>

using kyotocabinet::PolyDB;

static KyotoPortServer* _Self = NULL;

KyotoPortServer::KyotoPortServer() : 
	PortServer(),
	_RequestQueue(NULL) ,
	_ResponseQueue(NULL)
{
	_Self = this;
}
KyotoPortServer::~KyotoPortServer() {
	/*
	delete _RequestQueue;
	delete _ResponseQueue;
	*/
}

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

int KyotoPortServer::GetCommandId(const byte* packet, int packet_len) {
	if (packet_len < 4) {
		return -1;
	}
	byte l, h;
	l = packet[2];
	h = packet[3];
	int command_id = l + (h << 8);
	return command_id;
}

int KyotoPortServer::ReadKPSOptions() {
	bool keep_reading = true;
	do {
		byte* packet = NULL;
		int packet_len = recv_packet(&packet);
		
		assert(packet_len != -1);
		
		int pdu_type = GetPduType(packet, packet_len);
		int command_id = GetCommandId(packet, packet_len);
		
		assert(pdu_type == 2);
		assert(command_id != -1);

		KPSSetOptionRequest_t * setOptionReq = NULL;
		asn_dec_rval_t dr =	ber_decode(0, &asn_DEF_KPSSetOptionRequest, (void**)&setOptionReq, packet + 4, packet_len - 4);
		assert(dr.code == RC_OK);

		long iOptCode = 0;
		asn_INTEGER2long(&setOptionReq->optCode, &iOptCode);

		switch (iOptCode) {
			case opt_ThreadPoolSize: {
				long iThrPoolSize;
				asn_INTEGER2long( setOptionReq->optValueInteger, &iThrPoolSize);

				_Options.ThreadPoolSize = iThrPoolSize;
				fprintf(dbgout, "\rKyotoPortServer::ReadKPSOptions() ThreadPoolSize = %d\n", iThrPoolSize);
				break;
			}
			case opt_DebugLogFile:
				freopen((char*)setOptionReq->optValueString->buf, "a", dbgout);
				fprintf(dbgout, "\rKyotoPortServer::ReadKPSOptions() redirecting stderr to '%s'\n", setOptionReq->optValueString->buf);
				break;
			case opt_EndOfOptions:
				fprintf(dbgout, "\rKyotoPortServer::ReadKPSOptions() end of options reached\n");
				keep_reading = false;
				break;
			default:
				fprintf(dbgout, "\rKyotoPortServer::ReadKPSOptions() Unknown optCode: %d\n", iOptCode);
				return 1;
		}
		
		asn_DEF_KPSSetOptionRequest.free_struct(&asn_DEF_KPSSetOptionRequest, setOptionReq, 0 /* free the pointer too */);
		
		free_packet(packet);
		packet = NULL;
	} while (keep_reading);
	
	return 0;
}

int KyotoPortServer::InitThreadPool() {
#ifndef NO_QUEUES
	_RequestQueue = new RG::TaskQueue(_Options.ThreadPoolSize);
	_ResponseQueue = new RG::TaskQueue(1);
#endif // NO_QUEUES
	return 0;
}

int KyotoPortServer::RecvLoop() {
	bool keep_running = true;
	do {
		byte* packet = NULL;
		int packet_len = recv_packet(&packet);
		assert(packet_len != -1);

		KPSTask* task = new KPSTask(this, packet, packet_len);

#ifndef NO_QUEUES
		_RequestQueue->AddTask(task);
#else
		task->Run();
		assert( task->ToBeDisposedByWorker() == true );
		delete task;
#endif // NO_QUEUES

	} while (keep_running);
	return 0;
}

int KyotoPortServer::Run() {
	assert( ReadKPSOptions() == 0 );
	assert( InitThreadPool() == 0 );

	return RecvLoop();
}

void KyotoPortServer::Respond(KPSResponse * responseTask) {
#ifndef NO_QUEUES
	_ResponseQueue->AddTask(responseTask);
#else
	responseTask->Run();
	assert( responseTask->ToBeDisposedByWorker() == true );
	delete responseTask;
#endif // NO_QUEUES
}

