#ifndef _DbSetTask_h
#define _DbSetTask_h

#include "kyoto_client.h"
#include "DbGenericTask.h"

namespace kyoto_client {
	class DbSetTask : public DbGenericTask {
	private:
		
	public:
		DbSetTask();
		virtual ~DbSetTask();
		
		
		virtual void Run();
	};
}

#endif // _DbSetTask_h