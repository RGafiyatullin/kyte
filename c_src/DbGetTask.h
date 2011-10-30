#ifndef _DbGetTask_h
#define _DbGetTask_h

#include "kyoto_client.h"
#include "DbGenericTask.h"

namespace kyoto_client {
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