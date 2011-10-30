#include "TaskQueue.h"
#include "ITask.h"
#include "Worker.h"
#include "Thread.h"


namespace RG {
	TaskQueue::TaskQueue(int workersCount) : _Head(NULL), _Tail(NULL), _WorkersCount(workersCount), _Shutdown(false) {
		Lock l(_Lock);
		_Workers = new Worker*[workersCount];
		_Threads = new Thread*[workersCount];
		for (int i = 0; i < workersCount; i++) {
			_Threads[i] = new Thread;
			_Workers[i] = new Worker(this, i);
			_Threads[i]->SetTask(_Workers[i]);
			_Threads[i]->Start();
		}
	}
	TaskQueue::~TaskQueue() {
		for (int i = 0; i < _WorkersCount; i++) {
			delete _Threads[i];
			_Threads[i] = NULL;
			delete _Workers[i];
			_Workers[i] = NULL;
		}
		delete [] _Threads;
		delete [] _Workers;
	}
	
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
		_Monitor.Pulse();
	}
	ITask* TaskQueue::FetchTask() {
		Lock l(_Lock);
		for (;;) {
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
			_Monitor.Wait(_Lock);
			if (_Shutdown) {
				return NULL;
			}
		}
	}
	void TaskQueue::Shutdown() {
		_Shutdown = true;
		for (int i = 0; i < _WorkersCount; i++) {
			_Workers[i]->Shutdown();
		}
		_Monitor.PulseAll();
		for (int i = 0; i < _WorkersCount; i++) {
			_Threads[i]->Join();
		}
		QueueLink* lnk = _Head;
		while (lnk) {
			ITask* task = lnk->Data;
			if ( task->ToBeDisposedByWorker() ) {
				delete task;
			}
			
			QueueLink* toDelete = lnk;
			lnk = lnk->Next;
			delete toDelete;
		}
	}
}

