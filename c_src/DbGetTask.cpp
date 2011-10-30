#include "DbGetTask.h"

namespace kyoto_client {
	DbGetTask::DbGetTask() {}
	DbGetTask::~DbGetTask() {}
	void DbGetTask::Run() {
		if (!EnsureDB()) return;
		
	}

	void DbGetTask::SetKey(ERL_NIF_TERM key) {
		_Key = enif_make_copy(Env(), key);
	}
}
