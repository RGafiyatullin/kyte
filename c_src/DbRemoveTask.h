#ifndef _DbRemoveTask_h
#define _DbRemoveTask_h

#include "kyoto_client.h"
#include "DbGenericTask.h"

namespace kyoto_client {
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