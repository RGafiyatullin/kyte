/**
 * This file is a part of Kyte released under the MIT licence.
 * See the LICENCE file for more information
 */


#include "DbClearTask.h"

namespace kyte {
	using kyotocabinet::PolyDB;

	DbClearTask::DbClearTask() {}
	DbClearTask::~DbClearTask() {}

	void DbClearTask::Run() {
		if (!EnsureDB()) return;
		
		if ( DB()->clear() ) {
			Reply( enif_make_atom(Env(), "ok") );
		}
		else {
			Reply( enif_make_tuple2(Env(),
				enif_make_atom(Env(), "error"),
				enif_make_string(Env(), DB()->error().name(), ERL_NIF_LATIN1 )
			) );
		}
	}
}
