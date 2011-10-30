#include "Thread.h"

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include "Thread.h"
#include "ITask.h"

namespace RG {
	Thread::Thread() 
		: _CurrentTask(NULL)
	{}
	Thread::~Thread() {}
	
	void Thread::SetTask(ITask* task) {
		_CurrentTask = task;
	}
	
	ThreadRoutineRetValue_t Thread::PThreadDispatch(void* v_thr) {
		Thread* thr = (Thread*)v_thr;
		thr->Run();
		return NULL;
	}
	
	void Thread::Start() {
		pthread_attr_t thrAttr;
		pthread_attr_init(&thrAttr);
		pthread_create(&_Thread, &thrAttr, &PThreadDispatch, (void*)this);
		pthread_attr_destroy(&thrAttr);
	}
	void Thread::Run() {
		if (!_CurrentTask) {
			printf("Thread::Run() : No task set!\n");
			return;
		}
		
		_CurrentTask->Run();
	}
	void Thread::Join() {
		void* retVal;
		pthread_join(_Thread, &retVal);
	}
}