#include "DbClearTask.h"

namespace kyte {
	using kyotocabinet::PolyDB;

	DbClearTask::DbClearTask() {}
	DbClearTask::~DbClearTask() {}

	void DbClearTask::Run() {
		if (!EnsureDB()) return;
		
		PolyDB* db = DB();
		if ( db->clear() ) {
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
