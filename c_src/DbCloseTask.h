#ifndef _DbCloseTask_h
#define _DbCloseTask_h

#include "kyoto_client.h"
#include "NifAsyncTask.h"

namespace kyoto_client {
	class DbCloseTask : public NifAsyncTask {
	private:
		int _DbIdx;
	public:
		DbCloseTask();
		virtual ~DbCloseTask();
		virtual void Run();

		void SetDbIdx(int dbIdx);
	};
}

#endif // _DbCloseTask_h
