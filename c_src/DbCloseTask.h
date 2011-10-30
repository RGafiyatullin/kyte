#ifndef _DbCloseTask_h
#define _DbCloseTask_h

#include "kyoto_client.h"
#include "DbGenericTask.h"

namespace kyoto_client {
	class DbCloseTask : public DbGenericTask {
	private:
	public:
		DbCloseTask();
		virtual ~DbCloseTask();
		virtual void Run();
	};
}

#endif // _DbCloseTask_h
