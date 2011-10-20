#include "StdAfx.h"
#include <Worker.h>
#include <assert.h>
#ifdef WIN32
	#include <windows.h>
	#define sleep Sleep
	#define ONE_SECOND 1000
#else
	#include <unistd.h>
	#include <stdlib.h>
	#include <stdio.h>
	#define ONE_SECOND 1
#endif
#include <TaskQueue.h>

namespace RG {
	Worker::Worker(TaskQueue* q, int idx) : _Q(q), _Idx(idx), _Shutdown(false) {
		printf("[Worker %i]::ctor \n", idx);
	}
	Worker::~Worker() {}
	
	void Worker::Run() {
		printf("[Worker %i] Run() begin\n", _Idx);
		
		while (!_Shutdown) {
			ITask* currentJob = _Q->FetchTask();
			assert(currentJob != NULL);
			currentJob->Run();
			if (currentJob->ToBeDisposedByWorker()) {
				delete currentJob;
			}
		}
		
		printf("[Worker %i] Run() end\n", _Idx);
	}
}
