#include <KPSTask.h>

#include <KyotoPortServer.h>
#include <kcpolydb.h>

using kyotocabinet::PolyDB;

enum PDUTypes {
	pdu_KPSBasicRequest = 0,
	pdu_KPSBasicResponse = 1,
	pdu_KPSSetOptionRequest = 2,
	pdu_KPSSetOptionResponse = 3,
	pdu_KPSDbOpenRequest = 4,
	pdu_KPSDbOpenResponse = 5,
	pdu_KPSDbCloseRequest = 6,
	pdu_KPSDbCloseResponse = 7,
	pdu_KPSDbClearRequest = 8,
	pdu_KPSDbClearResponse = 9,
	pdu_KPSDbCountRequest = 10,
	pdu_KPSDbCountResponse = 11,
	pdu_KPSDbSizeRequest = 12,
	pdu_KPSDbSizeResponse = 13,
	pdu_KPSDbSetRequest = 14,
	pdu_KPSDbSetResponse = 15,
	pdu_KPSDbGetRequest = 16,
	pdu_KPSDbGetResponse = 17,
	pdu_KPSDbRemoveRequest = 18,
	pdu_KPSDbRemoveResponse = 19
};

KPSResponse::KPSResponse(KyotoPortServer* kps, byte* packet, int packet_len) :
	ITask(),
	_KPS(kps),
	_Packet(packet),
	_PacketLen(packet_len)
{}
KPSResponse::~KPSResponse() {
	_KPS = NULL;
	delete [] _Packet;
	_PacketLen = 0;
}

void KPSResponse::Run() {
	_KPS->send_packet(_Packet, _PacketLen);
}
bool KPSResponse::ToBeDisposedByWorker() const {
	return true;
}





KPSTask::KPSTask(KyotoPortServer* kps, byte* packet, int packet_len) : 
	ITask(),
	_KPS(kps),
	_Packet(packet),
	_PacketLen(packet_len)
{}
KPSTask::~KPSTask() {
	_KPS->free_packet(_Packet);
	_Packet = NULL;
	_PacketLen = -1;
	_KPS = NULL;
}

void KPSTask::Run() {
	assert(_PacketLen >= 4);
	int pdu_type = KyotoPortServer::GetPduType(_Packet, _PacketLen);
	assert(pdu_type != -1);
	_CID[0] = _Packet[2];
	_CID[1] = _Packet[3];

	/*
cat ./temp  | sed -e 's/->//' | sed -e "s/'//g" | sed -e 's/)//' | sed -e 's/;//' | sed -e 's/\.//' \
| grep 'Request' | awk '{print $1}' \
| while read R; do 
	echo -ne "\t\tCASE_REQ_TYPE($R)\n"
done > ./auto.cpp

cat ./temp  | sed -e 's/->//' | sed -e "s/'//g" | sed -e 's/)//' | sed -e 's/;//' | sed -e 's/\.//' \
| grep 'Request' | awk '{print $1}' \
| while read R; do 
	echo -ne "\tvoid process_${R}(const $R_t * req);\n"
done > ./auto.cpp

cat ./temp  | sed -e 's/->//' | sed -e "s/'//g" | sed -e 's/)//' | sed -e 's/;//' | sed -e 's/\.//' \
| grep 'Request' | awk '{print $1}' \
| while read R; do 
	echo -ne "void KPSTask::process_${R}(const ${R}_t * req) {\n\tfprintf(stderr, \"\\\\rKPSTask::process_${R}\\\\n\");\n}\n"
done > ./auto.cpp

	*/

	#define CASE_REQ_TYPE(R) \
		case pdu_ ## R: { \
			R ## _t * req = NULL; \
			asn_dec_rval_t dr = ber_decode(0, &asn_DEF_ ## R, (void**)&req, _Packet + 4, _PacketLen - 4); \
			assert(dr.code == RC_OK); \
			process_ ## R (req); \
			asn_DEF_ ## R.free_struct(&asn_DEF_ ## R , req, 0); \
		}; break;

	switch (pdu_type) {
		CASE_REQ_TYPE(KPSBasicRequest)
		CASE_REQ_TYPE(KPSSetOptionRequest)
		CASE_REQ_TYPE(KPSDbOpenRequest)
		CASE_REQ_TYPE(KPSDbCloseRequest)
		CASE_REQ_TYPE(KPSDbClearRequest)
		CASE_REQ_TYPE(KPSDbCountRequest)
		CASE_REQ_TYPE(KPSDbSizeRequest)
		CASE_REQ_TYPE(KPSDbSetRequest)
		CASE_REQ_TYPE(KPSDbGetRequest)
		CASE_REQ_TYPE(KPSDbRemoveRequest)
		default:
			fprintf(stderr, "\rGot a PDU (type %d)\n", pdu_type);
		break;
	}
	#undef CASE_REQ_TYPE

}
bool KPSTask::ToBeDisposedByWorker() const {
	return true;
}



void KPSTask::process_KPSBasicRequest(const KPSBasicRequest_t * req) {
	fprintf(stderr, "\rKPSTask::process_KPSBasicRequest\n");
}
void KPSTask::process_KPSSetOptionRequest(const KPSSetOptionRequest_t * req) {
	fprintf(stderr, "\rKPSTask::process_KPSSetOptionRequest\n");
}
void KPSTask::process_KPSDbOpenRequest(const KPSDbOpenRequest_t * req) {
	fprintf(stderr, "\rKPSTask::process_KPSDbOpenRequest\n");

	char * dbFile = new char[req->dbFile.size + 1];
	memcpy(dbFile, req->dbFile.buf, req->dbFile.size);
	dbFile[req->dbFile.size] = '\0';

	PolyDB * db = new PolyDB;
	KPSDbOpenResponse_t response = {0};
	
	INTEGER_t rc = {0};
	INTEGER_t dbHandle = {0};
	OCTET_STRING_t errorDescription = {NULL};

	fprintf(stderr, "\rKPSTask::process_KPSDbOpenRequest prior to real open\n");
	if ( db->open(dbFile, PolyDB::OCREATE | PolyDB::OWRITER) ) {
		fprintf(stderr, "\rKPSTask::process_KPSDbOpenRequest open succeeded\n");
		
		asn_long2INTEGER(&rc, 0);
		response.rc = rc;
		
		asn_long2INTEGER(&dbHandle, (long)db);
		response.dbHandle = &dbHandle;
		fprintf(stderr, "\rKPSTask::process_KPSDbOpenRequest db handle = %d\n", (long)db);
	}
	else
	{
		fprintf(stderr, "\rKPSTask::process_KPSDbOpenRequest open failed\n");
		
		asn_long2INTEGER(&rc, 1);
		response.rc = rc;

		int fromBufRet = OCTET_STRING_fromBuf(&errorDescription, db->error().name(), -1);
		assert(fromBufRet == 0);
		response.errorDescription = &errorDescription;
	}
	
	uint8_t* buffer = new uint8_t[ENC_BUFF_SIZE];
	buffer[0] = 5;
	buffer[1] = 0;
	buffer[2] = _CID[0];
	buffer[3] = _CID[1];
	asn_enc_rval_t er = der_encode_to_buffer(&asn_DEF_KPSSetOptionResponse, (void*)&response, buffer + 4, ENC_BUFF_SIZE - 4);

	assert(er.encoded != -1);
	fprintf(stderr, "\rKPSTask::process_KPSDbOpenRequest er.encoded = %d\n", er.encoded);
	_KPS->Respond(new KPSResponse(_KPS, buffer, er.encoded + 4));

	delete [] dbFile;
}
void KPSTask::process_KPSDbCloseRequest(const KPSDbCloseRequest_t * req) {
	fprintf(stderr, "\rKPSTask::process_KPSDbCloseRequest\n");
}
void KPSTask::process_KPSDbClearRequest(const KPSDbClearRequest_t * req) {
	fprintf(stderr, "\rKPSTask::process_KPSDbClearRequest\n");
}
void KPSTask::process_KPSDbCountRequest(const KPSDbCountRequest_t * req) {
	fprintf(stderr, "\rKPSTask::process_KPSDbCountRequest\n");
}
void KPSTask::process_KPSDbSizeRequest(const KPSDbSizeRequest_t * req) {
	fprintf(stderr, "\rKPSTask::process_KPSDbSizeRequest\n");
}
void KPSTask::process_KPSDbSetRequest(const KPSDbSetRequest_t * req) {
	fprintf(stderr, "\rKPSTask::process_KPSDbSetRequest\n");
}
void KPSTask::process_KPSDbGetRequest(const KPSDbGetRequest_t * req) {
	fprintf(stderr, "\rKPSTask::process_KPSDbGetRequest\n");
}
void KPSTask::process_KPSDbRemoveRequest(const KPSDbRemoveRequest_t * req) {
	fprintf(stderr, "\rKPSTask::process_KPSDbRemoveRequest\n");
}


