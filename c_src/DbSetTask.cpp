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

#include "DbSetTask.h"

namespace kyte {
	DbSetTask::DbSetTask() {}
	DbSetTask::~DbSetTask() {}
	void DbSetTask::Run() {
		if (!EnsureDB()) return;

		ErlNifBinary binKey, binValue;
		assert( enif_inspect_iolist_as_binary(Env(), _Key, &binKey) == true );
		assert( enif_inspect_iolist_as_binary(Env(), _Value, &binValue) == true );

		if (DB()->set( (char*)binKey.data, binKey.size, (char*)binValue.data, binValue.size )) {
			Reply( enif_make_atom(Env(), "ok") );
		}
		else {
			Reply( enif_make_tuple2(Env(),
				enif_make_atom(Env(), "error"),
				enif_make_string(Env(), DB()->error().name(), ERL_NIF_LATIN1)
			) );
		}
	}

	void DbSetTask::SetKey(ERL_NIF_TERM key) {
		_Key = enif_make_copy(Env(), key);
	}
	void DbSetTask::SetValue(ERL_NIF_TERM value) {
		_Value = enif_make_copy(Env(), value);
	}
}
