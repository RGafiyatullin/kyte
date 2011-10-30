#include "DbOpenTask.h"

#include <stdio.h>
#include <string.h>

namespace kyoto_client {
	using kyotocabinet::PolyDB;

	DbOpenTask::DbOpenTask() {
		_DbFile[0] = '\0';
	}
	DbOpenTask::~DbOpenTask() {}

	void DbOpenTask::Run() {
		fprintf(stderr, "DbOpenTask::Run() enter\r\n");
		PolyDB* db = new PolyDB;
		fprintf(stderr, "DbOpenTask::Run() 1\r\n");
		int dbPos = place_to_the_pool(db, _OpenDatabases, MAX_OPEN_DBS);
		fprintf(stderr, "DbOpenTask::Run() 2\r\n");
		if ( dbPos == -1 ) {
			fprintf(stderr, "DbOpenTask::Run() 2.1\r\n");
			delete db;
			Reply( enif_make_tuple2(Env(), 
				enif_make_atom(Env(), "error"), 
				enif_make_atom(Env(), "max_db_count_reached")
			) );
		}
		else {
			fprintf(stderr, "DbOpenTask::Run() 2.2 '%s'\r\n", _DbFile);
			if ( db->open(_DbFile, PolyDB::OCREATE | PolyDB::OWRITER) ) {
				fprintf(stderr, "DbOpenTask::Run() 2.2.1\r\n");
				Reply( enif_make_tuple2(Env(),
					enif_make_atom(Env(), "ok"),
					enif_make_int(Env(), dbPos)
				) );
			}
			else {
				fprintf(stderr, "DbOpenTask::Run() 2.2.2\r\n");
				_OpenDatabases[dbPos] = NULL;
				Reply( enif_make_tuple2(Env(), 
					enif_make_atom(Env(), "error"), 
					enif_make_string(Env(), db->error().name(), ERL_NIF_LATIN1)
				) );
				delete db;
			}
		}
		fprintf(stderr, "DbOpenTask::Run() leave\r\n");
	}
	void DbOpenTask::SetDbFile(const char* file) {
		strlcpy(_DbFile, file, MAX_PATH_LEN);
	}

}