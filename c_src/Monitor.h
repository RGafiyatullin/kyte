/**
 * This file is a part of Kyte released under the MIT licence.
 * See the LICENCE file for more information
 */

#ifndef _RG_Monitor_h
#define _RG_Monitor_h

#include <pthread.h>
#include "Mutex.h"

namespace RG {
	class Monitor {
	private:
		pthread_condattr_t _CVA;
		pthread_cond_t _CV;
	public:
		Monitor();
		virtual ~Monitor();

		void Wait(Mutex& m);
		void Pulse();
		void PulseAll();
	};
}

#endif // _RG_Monitor_h