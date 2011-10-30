#ifndef _kyte_h
#define _kyte_h

#include <unistd.h>
#include <stdio.h>

#define MAX_THR_POOLS 16
#define MAX_PATH_LEN  512
#define MAX_OPEN_DBS  128

template <typename El_t>
static int place_to_the_pool(El_t* item, El_t** pool, int poolSize) {
	fprintf(stderr, "place_to_the_pool<> enter\r\n");
	for (int i = 0; i < poolSize; i++) {
		fprintf(stderr, "place_to_the_pool<> checking %d\r\n", i);
		if ( pool[i] == NULL ) {
			fprintf(stderr, "place_to_the_pool<> free %d\r\n", i);
			pool[i] = item;
			return i;
		}
	}
	fprintf(stderr, "place_to_the_pool<> no free slot\r\n");
	return -1;
}


#endif // _kyte_h