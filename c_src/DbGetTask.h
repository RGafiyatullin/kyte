#ifndef _DbGetTask_h
#define _DbGetTask_h

#include "kyte.h"
#include "DbGenericTask.h"

namespace kyte {
	class DbGetTask : public DbGenericTask {
	private:
		ERL_NIF_TERM _Key;
	public:
		DbGetTask();
		virtual ~DbGetTask();

		void SetKey(ERL_NIF_TERM key);

		virtual void Run();
	};
}

#endif // _DbGetTask_h