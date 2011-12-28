/**
 * This file is a part of Kyte released under the MIT licence.
 * See the LICENCE file for more information
 */

#ifndef _RG_Worker_h
#define _RG_Worker_h

#include "ITask.h"

namespace RG {
	class TaskQueue;
	
	class Worker : public ITask {
	private:
		int _Idx;
		TaskQueue* _Q;
		bool _Shutdown;
	public:
		Worker(TaskQueue* q, int idx);
		virtual ~Worker();
		
		void Run();
		void Shutdown();
	};
}

#endif // _RG_Worker_h

