# echo -n "PS_SOURCES = "; cat ./KyotoPS.asn1 | grep '::=' | grep -v DEFINITIONS | awk '{print $1}' | while read Name; do echo -n "$Name.c "; done
# echo -n "PS_HEADERS = "; cat ./KyotoPS.asn1 | grep '::=' | grep -v DEFINITIONS | awk '{print $1}' | while read Name; do echo -n "$Name.h "; done
# echo -n "PS_OBJECTS = "; cat ./KyotoPS.asn1 | grep '::=' | grep -v DEFINITIONS | awk '{print $1}' | while read Name; do echo -n "$Name.o "; done
# cat ./KyotoPS.asn1 | grep '::=' | grep -v DEFINITIONS | awk '{print $1}' | while read Name; do echo -ne "$Name.o: $Name.c\n\t\$(CC) \$(CC_FLAGS) $Name.c\n\n"; done




ASN1C = asn1c
ASN_FLAGS = 

CC = gcc -c
CXX = g++ -c
CC_FLAGS = -I.
CXX_FLAGS = -I.
AR = ar

TOUCH = touch
RM_F = rm -f

LIB = generated.a

PS_SOURCES = KPSGenericResponse.c KPSSetOptionRequest.c KPSSetOptionResponse.c KPSOpenDBRequest.c KPSOpenDBResponse.c
PS_HEADERS = KPSGenericResponse.h KPSSetOptionRequest.h KPSSetOptionResponse.h KPSOpenDBRequest.h KPSOpenDBResponse.h
PS_OBJECTS = KPSGenericResponse.o KPSSetOptionRequest.o KPSSetOptionResponse.o KPSOpenDBRequest.o KPSOpenDBResponse.o

GENERATED_SOURCES = $(PS_SOURCES) BIT_STRING.c INTEGER.c NativeEnumerated.c NativeInteger.c OCTET_STRING.c asn_codecs_prim.c ber_decoder.c ber_tlv_length.c ber_tlv_tag.c constr_SEQUENCE.c constr_TYPE.c constraints.c converter-sample.c der_encoder.c per_decoder.c per_encoder.c per_support.c xer_decoder.c xer_encoder.c xer_support.c

GENERATED_HEADERS = $(PS_HEADERS) BIT_STRING.h INTEGER.h NativeEnumerated.h NativeInteger.h OCTET_STRING.h asn_application.h asn_codecs.h asn_codecs_prim.h asn_internal.h asn_system.h ber_decoder.h ber_tlv_length.h ber_tlv_tag.h constr_SEQUENCE.h constr_TYPE.h constraints.h der_encoder.h per_decoder.h per_encoder.h per_support.h xer_decoder.h xer_encoder.h xer_support.h

OBJECTS = $(PS_OBJECTS) BIT_STRING.o INTEGER.o NativeEnumerated.o NativeInteger.o OCTET_STRING.o asn_codecs_prim.o ber_decoder.o ber_tlv_length.o ber_tlv_tag.o constr_SEQUENCE.o constr_TYPE.o constraints.o converter-sample.o der_encoder.o per_decoder.o per_encoder.o per_support.o xer_decoder.o xer_encoder.o xer_support.o



all: $(LIB)

clean:
	$(RM_F) $(OBJECTS)
	$(RM_F) run-asn1c
	$(RM_F) Makefile.am.sample
	$(RM_F) $(GENERATED_SOURCES)
	$(RM_F) $(GENERATED_HEADERS)
	$(RM_F) $(LIB)

run-asn1c: ../KyotoPS.asn1
	$(ASN1C) $(ASN_FLAGS) ../KyotoPS.asn1
	$(TOUCH) run-asn1c

$(GENERATED_SOURCES) $(GENERATED_HEADERS): run-asn1c

$(LIB): $(OBJECTS)
	ar -r $(LIB) $(OBJECTS)


KPSGenericResponse.o: KPSGenericResponse.c
	$(CC) $(CC_FLAGS) KPSGenericResponse.c

KPSSetOptionRequest.o: KPSSetOptionRequest.c
	$(CC) $(CC_FLAGS) KPSSetOptionRequest.c

KPSSetOptionResponse.o: KPSSetOptionResponse.c
	$(CC) $(CC_FLAGS) KPSSetOptionResponse.c

KPSOpenDBRequest.o: KPSOpenDBRequest.c
	$(CC) $(CC_FLAGS) KPSOpenDBRequest.c

KPSOpenDBResponse.o: KPSOpenDBResponse.c
	$(CC) $(CC_FLAGS) KPSOpenDBResponse.c


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
