#include <stdio.h>	/* for stdout */
#include <stdlib.h>	/* for malloc() */
#include <assert.h>	/* for run-time control */

#include <KPSSetOptionRequest.h>
#include <KPSSetOptionResponse.h>

int main(int, char**) {
	int ret;

	KPSSetOptionRequest_t * setOptionReq = new KPSSetOptionRequest_t;
	
	INTEGER_t optCode;
	asn_long2INTEGER(&optCode, 3);
	setOptionReq->optCode = optCode;

	char* sOptValue = "test-value hooray!";
	OCTET_STRING_t optValue = {NULL};
	ret = OCTET_STRING_fromBuf(&optValue, sOptValue, -1);
	assert(ret == 0);
	
	setOptionReq->optValue = optValue;
	
	uint8_t* buffer = new uint8_t[1024];
	asn_enc_rval_t er = der_encode_to_buffer(&asn_DEF_KPSSetOptionRequest, setOptionReq, buffer, 1024);
	
	assert(er.encoded != -1);

	delete setOptionReq;

	printf("\n[");
	for (int i = 0; i <= er.encoded; i++) {
		printf("%u", buffer[i]);
		if (i != er.encoded)
			printf(", ");
	}
	printf("];\n");

	printf("ENCODING SUCCESS!!! er.encoded = %d\n", er.encoded);

	setOptionReq = NULL;
	asn_dec_rval_t dr =	ber_decode(0, &asn_DEF_KPSSetOptionRequest, (void**)&setOptionReq, buffer, 1024);

	assert(dr.code == RC_OK);
	printf("DECODING SUCCESS!!! dr.code = RC_OK\n");

	long iOptCode = 0;
	asn_INTEGER2long(&setOptionReq->optCode, &iOptCode);

	printf("OptCode:  %d\n", iOptCode);
	printf("OptValue: '%s'\n", setOptionReq->optValue.buf);
	
	delete [] buffer;
	return 0;
}

