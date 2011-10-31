#ifndef _DbClearTask_h
#define _DbClearTask_h

#include "kyte.h"
#include "DbGenericTask.h"

namespace kyte {
	class DbClearTask : public DbGenericTask {
	private:
	public:
		DbClearTask();
		virtual ~DbClearTask();
		virtual void Run();
	};
}

#endif // _DbClearTask_h
