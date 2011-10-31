#ifndef _DbSizeTask_h
#define _DbSizeTask_h

#include "kyte.h"
#include "DbGenericTask.h"

namespace kyte {
	class DbSizeTask : public DbGenericTask {
	private:
	public:
		DbSizeTask();
		virtual ~DbSizeTask();
		virtual void Run();
	};
}

#endif // _DbSizeTask_h
