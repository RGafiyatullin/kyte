/**
 * This file is a part of Kyte released under the MIT licence.
 * See the LICENCE file for more information
 */

#ifndef _DbOpenTask_h
#define _DbOpenTask_h

#include "kyte.h"
#include "NifAsyncTask.h"

namespace kyte {
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
