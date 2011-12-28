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

