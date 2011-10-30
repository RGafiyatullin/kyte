#include "NifAsyncTask.h"

namespace kyte {
	NifAsyncTask::NifAsyncTask() : _ErlEnv(NULL) {
		_ErlEnv = enif_alloc_env();
	}
	NifAsyncTask::~NifAsyncTask() {
		enif_free_env(_ErlEnv);
		_ErlEnv = NULL;
	}
	void NifAsyncTask::SetReplyTo(const ERL_NIF_TERM pid, const ERL_NIF_TERM ref) {
		_ReplyPid = enif_make_copy(Env(), pid);
		_ReplyRef = enif_make_copy(Env(), ref);
	}
	bool NifAsyncTask::ToBeDisposedByWorker() const {
		return true;
	}
	void NifAsyncTask::Reply(ERL_NIF_TERM replyTerm) {
		ErlNifPid pid;
		enif_get_local_pid(Env(), _ReplyPid, &pid);
		enif_send(NULL, &pid, Env(), enif_make_tuple2(Env(), _ReplyRef, replyTerm) );
	}
	void NifAsyncTask::SetOpenDBs(PolyDB** openDBs) {
		_OpenDatabases = openDBs;
	}
	ErlNifEnv* NifAsyncTask::Env() {
		return _ErlEnv;
	}
}