#ifndef _kyoto_client_h
#define _kyoto_client_h

#include <unistd.h>

#define MAX_THR_POOLS 16
#define MAX_PATH_LEN  512
#define MAX_OPEN_DBS  128

template <typename El_t>
static int place_to_the_pool(El_t* item, El_t** pool, int poolSize) {
	for (int i = 0; i < poolSize; i++) {
		if ( pool[i] == NULL ) {
			pool[i] = item;
			return i;
		}
	}
	return -1;
}


#endif // _kyoto_client_h