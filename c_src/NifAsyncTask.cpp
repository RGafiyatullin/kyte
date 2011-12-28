/**

Copyright (C) 2011 Roman Gafiyatullin <romko.goofique@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

*/

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