#pragma once
#ifndef _RG_Mutex_h
#define _RG_Mutex_h

#ifdef WIN32
	#include <windows.h>
	#define MutexHandle_t CRITICAL_SECTION
#else
	#include <stdio.h>
	#include <pthread.h>
	#define MutexHandle_t pthread_mutex_t
#endif

namespace RG {
	class Mutex {
	private:
		friend class Monitor;
		MutexHandle_t _Mutex;
	public:
		Mutex();
		virtual ~Mutex();
		
		void Lock();
		void Unlock();
	};
	
	class Lock {
	private:
		Mutex& _Mutex;
	public:
		Lock(Mutex& m) : _Mutex(m) {
			//printf("l:l()\n");
			_Mutex.Lock();
		}
		~Lock() {
			//printf("l:~l()\n");
			_Mutex.Unlock();
		}
		void Release() {
			_Mutex.Unlock();
		}
	};
}

#endif // _RG_Mutex_h