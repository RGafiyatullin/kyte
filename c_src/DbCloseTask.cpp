#include "DbCloseTask.h"

namespace kyoto_client {
	using kyotocabinet::PolyDB;

	DbCloseTask::DbCloseTask() {}
	DbCloseTask::~DbCloseTask() {}

	void DbCloseTask::Run() {
		if ( DB() == NULL) {
			Reply( enif_make_tuple2(Env(),
				enif_make_atom(Env(), "error"),
				enif_make_atom(Env(), "bad_db_idx")
			) );
			return;
		}
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
