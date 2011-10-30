#include "Worker.h"
#include <assert.h>

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#include "TaskQueue.h"

namespace RG {
	Worker::Worker(TaskQueue* q, int idx) : _Idx(idx), _Q(q), _Shutdown(false) {
	}
	Worker::~Worker() {}
	
	void Worker::Run() {
		while (!_Shutdown) {
			ITask* currentJob = _Q->FetchTask();
			if (currentJob != NULL) {
				currentJob->Run();
				if (currentJob->ToBeDisposedByWorker()) {
					delete currentJob;
				}
			}
		}
	}
	void Worker::Shutdown() {
		_Shutdown = true;
	}
}
