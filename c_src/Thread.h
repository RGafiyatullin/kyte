#pragma once

#ifndef _RG_Thread_h
#define _RG_Thread_h
#ifdef WIN32
	#include <windows.h>
	#define ThreadRoutineRetValue_t DWORD
	#define ThreadHandle_t HANDLE
#else
	#include <pthread.h>
	#define ThreadRoutineRetValue_t void*
	#define ThreadHandle_t pthread_t
#endif


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
#ifdef WIN32
		DWORD _ThreadID;
#endif
		ITask* _CurrentTask;
		
		void Run();
		static 	ThreadRoutineRetValue_t
#ifdef WIN32
		__stdcall
#endif
		PThreadDispatch(void* v_thr);
		
	};
}

#endif // _RG_Thread_h
