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

#include <kps.h>

namespace RG {
	Worker::Worker(TaskQueue* q, int idx) : _Q(q), _Idx(idx), _Shutdown(false) {
		//printf("[Worker %i]::ctor \n", idx);
	}
	Worker::~Worker() {}
	
	void Worker::Run() {
		//printf("[Worker %i] Run() begin\n", _Idx);
		
		while (!_Shutdown) {
			ITask* currentJob = _Q->FetchTask();
			assert(currentJob != NULL);
			fprintf(dbgout, "Worker starting job %p\n", currentJob);
			currentJob->Run();
			fprintf(dbgout, "Worker job complete %p\n", currentJob);
			if (currentJob->ToBeDisposedByWorker()) {
				fprintf(dbgout, "Worker deleting job %p\n", currentJob);
				delete currentJob;
			}
		}
		
		//printf("[Worker %i] Run() end\n", _Idx);
	}
}
