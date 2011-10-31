#include "DbCountTask.h"

namespace kyte {
	using kyotocabinet::PolyDB;

	DbCountTask::DbCountTask() {}
	DbCountTask::~DbCountTask() {}

	void DbCountTask::Run() {
		if (!EnsureDB()) return;
		
		int64_t count = DB()->count();
		if ( count != -1 ) {
			Reply( enif_make_tuple2(Env(),
				enif_make_atom(Env(), "ok"),
				enif_make_int64(Env(), count)
			) );
		}
		else {
			Reply( enif_make_tuple2(Env(),
				enif_make_atom(Env(), "error"),
				enif_make_string(Env(), DB()->error().name(), ERL_NIF_LATIN1 )
			) );
		}
	}
}
