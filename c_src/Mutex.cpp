#include "StdAfx.h"
#include <Mutex.h>

#ifdef WIN32

#else
#include <stdio.h>
#endif

namespace RG {
	Mutex::Mutex() {
#ifdef WIN32
		InitializeCriticalSection(&_Mutex);
#else
		//printf("m\n");
		pthread_mutexattr_t attr;
		pthread_mutexattr_init(&attr);
		pthread_mutex_init(&_Mutex, &attr);
		pthread_mutexattr_destroy(&attr);
#endif
	}
	Mutex::~Mutex() {
#ifdef WIN32
		DeleteCriticalSection(&_Mutex);
#else
		//printf("~m\n");
		pthread_mutex_destroy(&_Mutex);
#endif
	}
	
	void Mutex::Lock() {
#ifdef WIN32
		EnterCriticalSection(&_Mutex);
#else
		//printf("> m:l()\n");
		pthread_mutex_lock(&_Mutex);
		//printf("< m:l()\n");
#endif
	}
	void Mutex::Unlock() {
#ifdef WIN32
		LeaveCriticalSection(&_Mutex);
#else
		//printf("> m:u() \n");
		pthread_mutex_unlock(&_Mutex);
		//printf("< m:u() \n");
#endif
	}
}
