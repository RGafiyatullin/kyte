/**
 * This file is a part of Kyte released under the MIT licence.
 * See the LICENCE file for more information
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
