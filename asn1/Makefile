ASN1C = asn1c
ASN_FLAGS = 

CC = gcc -c
CXX = g++ -c
CC_FLAGS = -I.
CXX_FLAGS = -I.
AR = ar

TOUCH = touch
RM_F = rm -f

LIB = kpsasn1.a

ASN_SOURCE = ./KyotoPS.asn1
PS_SOURCES = KPSCommandId.c KPSDbHandle.c KPSBasicRequest.c KPSBasicResponse.c KPSSetOptionRequest.c KPSSetOptionResponse.c KPSDbOpenRequest.c KPSDbOpenResponse.c KPSDbCloseRequest.c KPSDbCloseResponse.c KPSDbClearRequest.c KPSDbClearResponse.c KPSDbCountRequest.c KPSDbCountResponse.c KPSDbSizeRequest.c KPSDbSizeResponse.c KPSDbSetRequest.c KPSDbSetResponse.c KPSDbGetRequest.c KPSDbGetResponse.c KPSDbRemoveRequest.c KPSDbRemoveResponse.c 
PS_HEADERS = KPSCommandId.h KPSDbHandle.h KPSBasicRequest.h KPSBasicResponse.h KPSSetOptionRequest.h KPSSetOptionResponse.h KPSDbOpenRequest.h KPSDbOpenResponse.h KPSDbCloseRequest.h KPSDbCloseResponse.h KPSDbClearRequest.h KPSDbClearResponse.h KPSDbCountRequest.h KPSDbCountResponse.h KPSDbSizeRequest.h KPSDbSizeResponse.h KPSDbSetRequest.h KPSDbSetResponse.h KPSDbGetRequest.h KPSDbGetResponse.h KPSDbRemoveRequest.h KPSDbRemoveResponse.h 
PS_OBJECTS = KPSCommandId.o KPSDbHandle.o KPSBasicRequest.o KPSBasicResponse.o KPSSetOptionRequest.o KPSSetOptionResponse.o KPSDbOpenRequest.o KPSDbOpenResponse.o KPSDbCloseRequest.o KPSDbCloseResponse.o KPSDbClearRequest.o KPSDbClearResponse.o KPSDbCountRequest.o KPSDbCountResponse.o KPSDbSizeRequest.o KPSDbSizeResponse.o KPSDbSetRequest.o KPSDbSetResponse.o KPSDbGetRequest.o KPSDbGetResponse.o KPSDbRemoveRequest.o KPSDbRemoveResponse.o


# ERL_PRODUCTS_IN_PLACES = ../include/KyotoPS.hrl ../src/KyotoPS.erl
# ERL_PRODUCTS = KyotoPS.hrl KyotoPS.erl KyotoPS.asn1db KyotoPS.beam


GENERATED_SOURCES = $(PS_SOURCES) BIT_STRING.c INTEGER.c NativeEnumerated.c NativeInteger.c OCTET_STRING.c asn_codecs_prim.c ber_decoder.c ber_tlv_length.c ber_tlv_tag.c constr_SEQUENCE.c constr_TYPE.c constraints.c converter-sample.c der_encoder.c per_decoder.c per_encoder.c per_support.c xer_decoder.c xer_encoder.c xer_support.c

GENERATED_HEADERS = $(PS_HEADERS) BIT_STRING.h INTEGER.h NativeEnumerated.h NativeInteger.h OCTET_STRING.h asn_application.h asn_codecs.h asn_codecs_prim.h asn_internal.h asn_system.h ber_decoder.h ber_tlv_length.h ber_tlv_tag.h constr_SEQUENCE.h constr_TYPE.h constraints.h der_encoder.h per_decoder.h per_encoder.h per_support.h xer_decoder.h xer_encoder.h xer_support.h

OBJECTS = $(PS_OBJECTS) BIT_STRING.o INTEGER.o NativeEnumerated.o NativeInteger.o OCTET_STRING.o asn_codecs_prim.o ber_decoder.o ber_tlv_length.o ber_tlv_tag.o constr_SEQUENCE.o constr_TYPE.o constraints.o converter-sample.o der_encoder.o per_decoder.o per_encoder.o per_support.o xer_decoder.o xer_encoder.o xer_support.o

all: $(LIB)

# $(ERL_PRODUCTS_IN_PLACES): $(ERL_PRODUCTS)
# 	cp KyotoPS.hrl ../include/KyotoPS.hrl
# 	cp KyotoPS.erl ../src/KyotoPS.erl

# $(ERL_PRODUCTS): $(ASN_SOURCE)
# 	erlc $(ASN_SOURCE)

clean:
	$(RM_F) $(OBJECTS)
	$(RM_F) run-asn1c
	$(RM_F) Makefile.am.sample
	$(RM_F) $(GENERATED_SOURCES)
	$(RM_F) $(GENERATED_HEADERS)
	$(RM_F) $(LIB)



run-asn1c: $(ASN_SOURCE)
	$(ASN1C) $(ASN_FLAGS) $(ASN_SOURCE)
	$(TOUCH) run-asn1c

$(GENERATED_SOURCES) $(GENERATED_HEADERS): run-asn1c

$(LIB): $(OBJECTS)
	ar -r $(LIB) $(OBJECTS)




KPSCommandId.o: KPSCommandId.c
	$(CC) $(CC_FLAGS) KPSCommandId.c

KPSDbHandle.o: KPSDbHandle.c
	$(CC) $(CC_FLAGS) KPSDbHandle.c

KPSBasicRequest.o: KPSBasicRequest.c
	$(CC) $(CC_FLAGS) KPSBasicRequest.c

KPSBasicResponse.o: KPSBasicResponse.c
	$(CC) $(CC_FLAGS) KPSBasicResponse.c

KPSSetOptionRequest.o: KPSSetOptionRequest.c
	$(CC) $(CC_FLAGS) KPSSetOptionRequest.c

KPSSetOptionResponse.o: KPSSetOptionResponse.c
	$(CC) $(CC_FLAGS) KPSSetOptionResponse.c

KPSDbOpenRequest.o: KPSDbOpenRequest.c
	$(CC) $(CC_FLAGS) KPSDbOpenRequest.c

KPSDbOpenResponse.o: KPSDbOpenResponse.c
	$(CC) $(CC_FLAGS) KPSDbOpenResponse.c

KPSDbCloseRequest.o: KPSDbCloseRequest.c
	$(CC) $(CC_FLAGS) KPSDbCloseRequest.c

KPSDbCloseResponse.o: KPSDbCloseResponse.c
	$(CC) $(CC_FLAGS) KPSDbCloseResponse.c

KPSDbClearRequest.o: KPSDbClearRequest.c
	$(CC) $(CC_FLAGS) KPSDbClearRequest.c

KPSDbClearResponse.o: KPSDbClearResponse.c
	$(CC) $(CC_FLAGS) KPSDbClearResponse.c

KPSDbCountRequest.o: KPSDbCountRequest.c
	$(CC) $(CC_FLAGS) KPSDbCountRequest.c

KPSDbCountResponse.o: KPSDbCountResponse.c
	$(CC) $(CC_FLAGS) KPSDbCountResponse.c

KPSDbSizeRequest.o: KPSDbSizeRequest.c
	$(CC) $(CC_FLAGS) KPSDbSizeRequest.c

KPSDbSizeResponse.o: KPSDbSizeResponse.c
	$(CC) $(CC_FLAGS) KPSDbSizeResponse.c

KPSDbSetRequest.o: KPSDbSetRequest.c
	$(CC) $(CC_FLAGS) KPSDbSetRequest.c

KPSDbSetResponse.o: KPSDbSetResponse.c
	$(CC) $(CC_FLAGS) KPSDbSetResponse.c

KPSDbGetRequest.o: KPSDbGetRequest.c
	$(CC) $(CC_FLAGS) KPSDbGetRequest.c

KPSDbGetResponse.o: KPSDbGetResponse.c
	$(CC) $(CC_FLAGS) KPSDbGetResponse.c

KPSDbRemoveRequest.o: KPSDbRemoveRequest.c
	$(CC) $(CC_FLAGS) KPSDbRemoveRequest.c

KPSDbRemoveResponse.o: KPSDbRemoveResponse.c
	$(CC) $(CC_FLAGS) KPSDbRemoveResponse.c
	




BIT_STRING.o: BIT_STRING.c
	$(CC) $(CC_FLAGS) BIT_STRING.c

INTEGER.o: INTEGER.c
	$(CC) $(CC_FLAGS) INTEGER.c

NativeEnumerated.o: NativeEnumerated.c
	$(CC) $(CC_FLAGS) NativeEnumerated.c

NativeInteger.o: NativeInteger.c
	$(CC) $(CC_FLAGS) NativeInteger.c

OCTET_STRING.o: OCTET_STRING.c
	$(CC) $(CC_FLAGS) OCTET_STRING.c

asn_codecs_prim.o: asn_codecs_prim.c
	$(CC) $(CC_FLAGS) asn_codecs_prim.c

ber_decoder.o: ber_decoder.c
	$(CC) $(CC_FLAGS) ber_decoder.c

ber_tlv_length.o: ber_tlv_length.c
	$(CC) $(CC_FLAGS) ber_tlv_length.c

ber_tlv_tag.o: ber_tlv_tag.c
	$(CC) $(CC_FLAGS) ber_tlv_tag.c

constr_SEQUENCE.o: constr_SEQUENCE.c
	$(CC) $(CC_FLAGS) constr_SEQUENCE.c

constr_TYPE.o: constr_TYPE.c
	$(CC) $(CC_FLAGS) constr_TYPE.c

constraints.o: constraints.c
	$(CC) $(CC_FLAGS) constraints.c

converter-sample.o: converter-sample.c
	$(CC) $(CC_FLAGS) converter-sample.c

der_encoder.o: der_encoder.c
	$(CC) $(CC_FLAGS) der_encoder.c

per_decoder.o: per_decoder.c
	$(CC) $(CC_FLAGS) per_decoder.c

per_encoder.o: per_encoder.c
	$(CC) $(CC_FLAGS) per_encoder.c

per_support.o: per_support.c
	$(CC) $(CC_FLAGS) per_support.c

xer_decoder.o: xer_decoder.c
	$(CC) $(CC_FLAGS) xer_decoder.c

xer_encoder.o: xer_encoder.c
	$(CC) $(CC_FLAGS) xer_encoder.c

xer_support.o: xer_support.c
	$(CC) $(CC_FLAGS) xer_support.c




