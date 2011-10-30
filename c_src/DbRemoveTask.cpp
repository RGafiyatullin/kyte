#include "DbRemoveTask.h"

namespace kyte {
	DbRemoveTask::DbRemoveTask() {}
	DbRemoveTask::~DbRemoveTask() {}
	void DbRemoveTask::Run() {
		if (!EnsureDB()) return;
		
		ErlNifBinary binKey;
		assert( enif_inspect_iolist_as_binary(Env(), _Key, &binKey) == true );

		if ( DB()->remove((char*)binKey.data, binKey.size) )
			Reply( enif_make_atom(Env(), "ok") );
		else
			Reply( enif_make_tuple2(Env(),
				enif_make_atom(Env(), "error"),
				enif_make_string(Env(), DB()->error().name(), ERL_NIF_LATIN1)
			) );
	}

	void DbRemoveTask::SetKey(ERL_NIF_TERM key) {
		_Key = enif_make_copy(Env(), key);
	}
}
