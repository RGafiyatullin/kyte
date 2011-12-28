/**
 * This file is a part of Kyte released under the MIT licence.
 * See the LICENCE file for more information
 */

#include "DbCloseTask.h"

namespace kyte {
	using kyotocabinet::PolyDB;

	DbCloseTask::DbCloseTask() {}
	DbCloseTask::~DbCloseTask() {}

	void DbCloseTask::Run() {
		if (!EnsureDB()) return;
		
		PolyDB* db = DB();
		if ( db->close() ) {
			DB() = NULL;
			delete db;
			Reply( enif_make_atom(Env(), "ok") );
		}
		else {
			Reply( enif_make_tuple2(Env(),
				enif_make_atom(Env(), "error"),
				enif_make_string(Env(), db->error().name(), ERL_NIF_LATIN1 )
			) );
		}
	}
}
