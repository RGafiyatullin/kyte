#include "DbSizeTask.h"

namespace kyte {
	using kyotocabinet::PolyDB;

	DbSizeTask::DbSizeTask() {}
	DbSizeTask::~DbSizeTask() {}

	void DbSizeTask::Run() {
		if (!EnsureDB()) return;
		
		int64_t size = DB()->size();
		if ( size != -1 ) {
			Reply( enif_make_tuple2(Env(),
				enif_make_atom(Env(), "ok"),
				enif_make_int64(Env(), size)
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
