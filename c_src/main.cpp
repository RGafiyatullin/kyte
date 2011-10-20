
#include <stdio.h>

#include "KyotoPortServer.h"

int main(int, char**) {
	perror("KPS: main enter\n");
	KyotoPortServer portSrv;
	portSrv.init();
	return portSrv.run();
}

