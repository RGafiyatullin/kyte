#ifndef _DbCloseTask_h
#define _DbCloseTask_h

#include "kyte.h"
#include "DbGenericTask.h"

namespace kyte {
	class DbCloseTask : public DbGenericTask {
	private:
	public:
		DbCloseTask();
		virtual ~DbCloseTask();
		virtual void Run();
	};
}

#endif // _DbCloseTask_h
