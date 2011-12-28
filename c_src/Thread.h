/**
 * This file is a part of Kyte released under the MIT licence.
 * See the LICENCE file for more information
 */

#ifndef _RG_Thread_h
#define _RG_Thread_h
#include <pthread.h>
#define ThreadRoutineRetValue_t void*
#define ThreadHandle_t pthread_t

namespace RG {
	class ITask;
	
	class Thread {
	public:
		Thread();
		virtual ~Thread();
		
		void SetTask(ITask* task);
		
		void Start();
		void Join();
	private:
		ThreadHandle_t _Thread;
		ITask* _CurrentTask;
		
		void Run();
		static 	ThreadRoutineRetValue_t PThreadDispatch(void* v_thr);
		
	};
}

#endif // _RG_Thread_h
