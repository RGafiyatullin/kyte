#include "DbGenericTask.h"

namespace kyoto_client {
	using kyotocabinet::PolyDB;

	DbGenericTask::DbGenericTask() {}
	DbGenericTask::~DbGenericTask() {}
	void DbGenericTask::SetDbIdx(int idx) { _DbIdx = idx; }
	int DbGenericTask::GetDbIdx() const { return _DbIdx; }
	PolyDB*& DbGenericTask::DB() {
		return _OpenDatabases[GetDbIdx()];
	}

}
