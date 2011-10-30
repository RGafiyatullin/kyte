#include "DbGenericTask.h"

namespace kyte {
	using kyotocabinet::PolyDB;

	DbGenericTask::DbGenericTask() {}
	DbGenericTask::~DbGenericTask() {}
	void DbGenericTask::SetDbIdx(int idx) { _DbIdx = idx; }
	int DbGenericTask::GetDbIdx() const { return _DbIdx; }
	PolyDB*& DbGenericTask::DB() {
		return _OpenDatabases[GetDbIdx()];
	}
	bool DbGenericTask::EnsureDB() {
		if (! DB() ) {
			Reply(enif_make_tuple2( Env(),
				enif_make_atom(Env(), "error"),
				enif_make_atom(Env(), "invalid_handle")
			));
			return false;
		}
		return true;
	}
}
