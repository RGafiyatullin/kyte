/**
 * This file is a part of Kyte released under the MIT licence.
 * See the LICENCE file for more information
 */

#ifndef _kyte_h
#define _kyte_h

#include <unistd.h>
#include <stdio.h>

#define MAX_THR_POOLS 16
#define MAX_PATH_LEN  512
#define MAX_OPEN_DBS  512

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


#endif // _kyte_h