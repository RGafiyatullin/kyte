/**
 * This file is a part of Kyte released under the MIT licence.
 * See the LICENCE file for more information
 */

#ifndef _NifAsyncTask_h
#define _NifAsyncTask_h

extern "C" {
	#include <erl_nif.h>
}

#include "ITask.h"

#include <kcpolydb.h>

namespace kyte {
	using kyotocabinet::PolyDB;

	class NifAsyncTask : public RG::ITask {
	private:
		ErlNifEnv* _ErlEnv;

		ERL_NIF_TERM _ReplyPid;
		ERL_NIF_TERM _ReplyRef;
	protected:
		PolyDB** _OpenDatabases;
	public:
		NifAsyncTask();
		virtual ~NifAsyncTask();

		void SetReplyTo(const ERL_NIF_TERM pid, const ERL_NIF_TERM ref);
		void SetOpenDBs(PolyDB** openDBs);

		virtual bool ToBeDisposedByWorker() const;
	protected:
		ErlNifEnv* Env();
		void Reply(ERL_NIF_TERM replyTerm);
	};
}

#endif // _NifAsyncTask_h
