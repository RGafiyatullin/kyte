#include "DbCloseTask.h"

namespace kyoto_client {
	using kyotocabinet::PolyDB;

	DbCloseTask::DbCloseTask() {}
	DbCloseTask::~DbCloseTask() {}

	void DbCloseTask::Run() {
		if ( DB() == NULL) {
			Reply( enif_make_tuple2(_ErlEnv,
				enif_make_atom(_ErlEnv, "error"),
				enif_make_atom(_ErlEnv, "bad_db_idx")
			) );
			return;
		}
		PolyDB* db = DB();
		if ( db->close() ) {
			DB() = NULL;
			delete db;
			Reply( enif_make_atom(_ErlEnv, "ok") );
		}
		else {
			Reply( enif_make_tuple2(_ErlEnv,
				enif_make_atom(_ErlEnv, "error"),
				enif_make_string(_ErlEnv, db->error().name(), ERL_NIF_LATIN1 )
			) );
		}
	}
}
