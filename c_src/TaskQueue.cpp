/**

Copyright (C) 2011 Roman Gafiyatullin <romko.goofique@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

*/

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
		_Tail = _Head = NULL;
	}
}

