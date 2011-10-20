#include "StdAfx.h"

#include <TaskQueue.h>
#include <ITask.h>
#include <Worker.h>
#include <Thread.h>


namespace RG {
	TaskQueue::TaskQueue(int workersCount) : _Head(NULL), _Tail(NULL), _WorkersCount(workersCount)  {
		Lock l(_Lock);
		printf("Initializing TaskQueue with %i workers\n", workersCount);
		_Workers = new Worker*[workersCount];
		_Threads = new Thread*[workersCount];
		for (int i = 0; i < workersCount; i++) {
			_Threads[i] = new Thread;
			_Workers[i] = new Worker(this, i);
			_Threads[i]->SetTask(_Workers[i]);
			_Threads[i]->Start();
		}
	}
	TaskQueue::~TaskQueue() {}
	
	void TaskQueue::AddTask(ITask* task) {
		Lock l(_Lock);
		
		if (_Head && _Tail) {
			QueueLink* exTail = _Tail;
			_Tail = new QueueLink;
			exTail->Next = _Tail;
			_Tail->Next = NULL;
			_Tail->Data = task;
		}
		else {
			_Head = _Tail = new QueueLink;
			_Head->Data = task;
			_Head->Next = NULL;
		}
	}
	ITask* TaskQueue::FetchTask() {
		Lock l(_Lock);
		
		if (_Head) {
			QueueLink* exHead = _Head;
			_Head = exHead->Next;
			
			ITask* task = exHead->Data;
			delete exHead;
			
			if (!_Head) {
				_Tail = NULL;
			}
			
			return task;
		}
		else {
			return NULL; // TO BE DONE
		}
	}
}

