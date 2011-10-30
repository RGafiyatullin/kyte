#include "Mutex.h"

#include <stdio.h>

namespace RG {
	Mutex::Mutex() {
		pthread_mutexattr_t attr;
		pthread_mutexattr_init(&attr);
		pthread_mutex_init(&_Mutex, &attr);
		pthread_mutexattr_destroy(&attr);
	}
	Mutex::~Mutex() {
		pthread_mutex_destroy(&_Mutex);
	}
	
	void Mutex::Lock() {
		pthread_mutex_lock(&_Mutex);
	}
	void Mutex::Unlock() {
		pthread_mutex_unlock(&_Mutex);
	}
}
