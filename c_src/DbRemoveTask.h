/**
 * This file is a part of Kyte released under the MIT licence.
 * See the LICENCE file for more information
 */

#ifndef _DbRemoveTask_h
#define _DbRemoveTask_h

#include "kyte.h"
#include "DbGenericTask.h"

namespace kyte {
	class DbRemoveTask : public DbGenericTask {
	private:
		ERL_NIF_TERM _Key;
	public:
		DbRemoveTask();
		virtual ~DbRemoveTask();

		void SetKey(ERL_NIF_TERM key);

		virtual void Run();
	};
}

#endif // _DbGetTask_h