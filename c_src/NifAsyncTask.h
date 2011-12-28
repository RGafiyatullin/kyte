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
