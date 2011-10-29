
#ifndef _RG_TaskQueue_h
#define _RG_TaskQueue_h

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

#include "Mutex.h"
#include "Monitor.h"

namespace RG {
	class ITask;
	class Worker;
	class Thread;
	
	class QueueLink {
	public:
		ITask* Data;
		QueueLink* Next;
	};
	
	class TaskQueue {
	private:
		QueueLink* _Head;
		QueueLink* _Tail;
		
		int _WorkersCount;
		Worker** _Workers;
		Thread** _Threads;
		
		Mutex _Lock;
		Monitor _Monitor;
		bool _Shutdown;
	public:
		TaskQueue(int workersCount) ;
		virtual ~TaskQueue();
		
		void AddTask(ITask* task);
		ITask* FetchTask();

		void Shutdown();
	};
}

#endif //_RG_TaskQueue_h

