#!/bin/bash

export LDFLAGS=""
export CXXFLAGS=""
export CXX="clang++"
SILENT="--silent"

HERE="$(dirname $0)"
cd "${HERE}"
KYOTO_PREFIX="$(pwd)/kyoto-root"

KYOTO=kyotocabinet-1.2.76

if [ ! -f "${KYOTO}.tar.gz" ]; then 
	wget -c "http://fallabs.com/kyotocabinet/pkg/${KYOTO}.tar.gz"
fi &&
if [ ! -d "${KYOTO}" ]; then
	tar xzf "${KYOTO}.tar.gz"
	(
		cd ${KYOTO}
		patch < ../kcthread.h.diff
		patch < ../kcthread.cc.diff
	)
fi &&
cd "${KYOTO}" &&
if [ ! -f Makefile ]; then
     ./configure --prefix "${KYOTO_PREFIX}"
else
    :
fi &&
if [ ! -d "$KYOTO_PREFIX" ]; then
	make $SILENT && make $SILENT install
fi


