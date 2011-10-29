#ifndef _NifAsyncTask_h
#define _NifAsyncTask_h

extern "C" {
	#include <erl_nif.h>
}

#include "ITask.h"

#include <kcpolydb.h>

namespace kyoto_client {
	using kyotocabinet::PolyDB;

	class NifAsyncTask : public RG::ITask {
	private:
		ERL_NIF_TERM _ReplyPid;
		ERL_NIF_TERM _ReplyRef;
	protected:
		ErlNifEnv* _ErlEnv;
		PolyDB** _OpenDatabases;
	public:
		NifAsyncTask();
		virtual ~NifAsyncTask();

		void SetReplyTo(const ERL_NIF_TERM pid, const ERL_NIF_TERM ref);
		void SetOpenDBs(PolyDB** openDBs);

		virtual bool ToBeDisposedByWorker() const;
	protected:
		void Reply(ERL_NIF_TERM replyTerm);
	};
}

#endif // _NifAsyncTask_h
