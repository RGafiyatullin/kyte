/**
 * This file is a part of Kyte released under the MIT licence.
 * See the LICENCE file for more information
 */

#include "DbGetTask.h"

#ifndef KYTE_MAX_RECORD_SIZE
#define KYTE_MAX_RECORD_SIZE 64000
#endif // KYTE_MAX_RECORD_SIZE

namespace kyte {
	DbGetTask::DbGetTask() {}
	DbGetTask::~DbGetTask() {}
	void DbGetTask::Run() {
		if (!EnsureDB()) return;
		ErlNifBinary binKey;
		assert( enif_inspect_iolist_as_binary( Env(), _Key, &binKey ) == true );

		ErlNifBinary binValue;
		if ( !enif_alloc_binary( KYTE_MAX_RECORD_SIZE, &binValue) ) {
			Reply( enif_make_tuple2( Env(),
				enif_make_atom(Env(), "error"),
				enif_make_string(Env(), "alloc", ERL_NIF_LATIN1)
			) );
		}
		
		int32_t size = DB()->get( (char*)binKey.data, binKey.size, (char*) binValue.data, binValue.size );

		if (size == -1) {
			enif_release_binary( &binValue );

			Reply( enif_make_tuple2( Env(),
				enif_make_atom(Env(), "error"),
				enif_make_string(Env(), DB()->error().name(), ERL_NIF_LATIN1)
			) );
		}
		else {
			Reply( enif_make_tuple2( Env(),
				enif_make_atom(Env(), "ok"),
				enif_make_binary(Env(), &binValue)
			) );
		}
	}

	void DbGetTask::SetKey(ERL_NIF_TERM key) {
		_Key = enif_make_copy(Env(), key);
	}
}
