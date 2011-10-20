#!/bin/bash

echo -n "PS_SOURCES = "; cat ../KyotoPS.asn1 | grep '::=' | grep -v DEFINITIONS | awk '{print $1}' | while read Name; do echo -n "$Name.c "; done
echo
echo -n "PS_HEADERS = "; cat ../KyotoPS.asn1 | grep '::=' | grep -v DEFINITIONS | awk '{print $1}' | while read Name; do echo -n "$Name.h "; done
echo
echo -n "PS_OBJECTS = "; cat ../KyotoPS.asn1 | grep '::=' | grep -v DEFINITIONS | awk '{print $1}' | while read Name; do echo -n "$Name.o "; done
echo
cat ../KyotoPS.asn1 | grep '::=' | grep -v DEFINITIONS | awk '{print $1}' | while read Name; do echo -ne "$Name.o: $Name.c\n\t\$(CC) \$(CC_FLAGS) $Name.c\n\n"; done