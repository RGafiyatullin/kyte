#ifndef _DbOpenTask_h
#define _DbOpenTask_h

#include "kyoto_client.h"
#include "NifAsyncTask.h"

namespace kyoto_client {
	class DbOpenTask : public NifAsyncTask {
	private:
		char _DbFile[MAX_PATH_LEN + 1];
	public:
		DbOpenTask();
		virtual ~DbOpenTask();
		virtual void Run();

		void SetDbFile(const char* file);
	};
}

#endif // _DbOpenTask_h
