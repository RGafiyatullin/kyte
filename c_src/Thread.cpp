
#include "StdAfx.h"

#include "Thread.h"

#ifdef WIN32
	
#else
	#include <stdlib.h>
	#include <stdio.h>
	#include <unistd.h>
#endif

#include <Thread.h>
#include <ITask.h>

namespace RG {
	Thread::Thread() 
		: _CurrentTask(NULL)
	{}
	Thread::~Thread() {}
	
	void Thread::SetTask(ITask* task) {
		_CurrentTask = task;
	}
	
	ThreadRoutineRetValue_t
#ifdef WIN32
	__stdcall
#endif
	Thread::PThreadDispatch(void* v_thr) {
		Thread* thr = (Thread*)v_thr;
		thr->Run();
		return NULL;
	}
	
	void Thread::Start() {
#ifdef WIN32
		_Thread = CreateThread(NULL, 0, &PThreadDispatch, (void*)this, 0, &_ThreadID);
#else
		pthread_attr_t thrAttr;
		pthread_attr_init(&thrAttr);
		pthread_create(&_Thread, &thrAttr, &PThreadDispatch, (void*)this);
		pthread_attr_destroy(&thrAttr);
#endif
	}
	void Thread::Run() {
		if (!_CurrentTask) {
			printf("Thread::Run() : No task set!\n");
			return;
		}
		
		_CurrentTask->Run();
	}
	void Thread::Join() {
#ifdef WIN32
		WaitForSingleObject(_Thread, 0 );
#else
		void* retVal;
		pthread_join(_Thread, &retVal);
#endif
	}
}