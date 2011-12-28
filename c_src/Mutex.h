/**
 * This file is a part of Kyte released under the MIT licence.
 * See the LICENCE file for more information
 */

#pragma once
#ifndef _RG_Mutex_h
#define _RG_Mutex_h

#include <stdio.h>
#include <pthread.h>
#define MutexHandle_t pthread_mutex_t

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
			_Mutex.Lock();
		}
		~Lock() {
			_Mutex.Unlock();
		}
		void Release() {
			_Mutex.Unlock();
		}
	};
}

#endif // _RG_Mutex_h