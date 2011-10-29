#include "NifAsyncTask.h"

namespace kyoto_client {
	NifAsyncTask::NifAsyncTask() : _ErlEnv(NULL) {
		_ErlEnv = enif_alloc_env();
	}
	NifAsyncTask::~NifAsyncTask() {
		enif_free_env(_ErlEnv);
		_ErlEnv = NULL;
	}
	void NifAsyncTask::SetReplyTo(const ERL_NIF_TERM pid, const ERL_NIF_TERM ref) {
		_ReplyPid = enif_make_copy(_ErlEnv, pid);
		_ReplyRef = enif_make_copy(_ErlEnv, ref);
	}
	bool NifAsyncTask::ToBeDisposedByWorker() const {
		return true;
	}
	void NifAsyncTask::Reply(ERL_NIF_TERM replyTerm) {
		ErlNifPid pid;
		enif_get_local_pid(_ErlEnv, _ReplyPid, &pid);
		enif_send(NULL, &pid, _ErlEnv, enif_make_tuple2(_ErlEnv, _ReplyRef, replyTerm) );
	}
	void NifAsyncTask::SetOpenDBs(PolyDB** openDBs) {
		_OpenDatabases = openDBs;
	}
}