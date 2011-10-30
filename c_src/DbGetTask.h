#ifndef _DbGetTask_h
#define _DbGetTask_h

#include "kyoto_client.h"
#include "DbGenericTask.h"

namespace kyoto_client {
	class DbGetTask : public DbGenericTask {
	private:

	public:
		DbGetTask();
		virtual ~DbGetTask();


		virtual void Run();
	};
}

#endif // _DbGetTask_h