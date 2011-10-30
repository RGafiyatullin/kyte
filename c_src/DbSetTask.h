#ifndef _DbSetTask_h
#define _DbSetTask_h

#include "kyoto_client.h"
#include "DbGenericTask.h"

namespace kyoto_client {
	class DbSetTask : public DbGenericTask {
	private:
		ERL_NIF_TERM _Key;
		ERL_NIF_TERM _Value;
	public:
		DbSetTask();
		virtual ~DbSetTask();
		
		void SetKey(ERL_NIF_TERM key);
		void SetValue(ERL_NIF_TERM value);
		
		virtual void Run();
	};
}

#endif // _DbSetTask_h