/**
 * This file is a part of Kyte released under the MIT licence.
 * See the LICENCE file for more information
 */

#ifndef _DbCountTask_h
#define _DbCountTask_h

#include "kyte.h"
#include "DbGenericTask.h"

namespace kyte {
	class DbCountTask : public DbGenericTask {
	private:
	public:
		DbCountTask();
		virtual ~DbCountTask();
		virtual void Run();
	};
}

#endif // _DbCountTask_h
