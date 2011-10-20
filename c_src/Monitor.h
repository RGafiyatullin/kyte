#ifndef _RG_Monitor_h
#define _RG_Monitor_h

#include <pthread.h>
#include <Mutex.h>

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
	};
}

#endif // _RG_Monitor_h