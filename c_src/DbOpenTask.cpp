/**
 * This file is a part of Kyte released under the MIT licence.
 * See the LICENCE file for more information
 */

#include "DbOpenTask.h"

#include <stdio.h>
#include <string.h>

namespace kyte {
	using kyotocabinet::PolyDB;

	DbOpenTask::DbOpenTask() {
		_DbFile[0] = '\0';
	}
	DbOpenTask::~DbOpenTask() {}

	void DbOpenTask::Run() {
		PolyDB* db = new PolyDB;
		int dbPos = place_to_the_pool(db, _OpenDatabases, MAX_OPEN_DBS);
		if ( dbPos == -1 ) {
			delete db;
			Reply( enif_make_tuple2(Env(),
				enif_make_atom(Env(), "error"),
				enif_make_atom(Env(), "max_db_count_reached")
			) );
		}
		else {
		  if ( db->open(_DbFile, PolyDB::OCREATE | PolyDB::OWRITER | PolyDB::OAUTOTRAN) ) {
				Reply( enif_make_tuple2(Env(),
					enif_make_atom(Env(), "ok"),
					enif_make_int(Env(), dbPos)
				) );
			}
			else {
				_OpenDatabases[dbPos] = NULL;
				Reply( enif_make_tuple2(Env(),
					enif_make_atom(Env(), "error"),
					enif_make_string(Env(), db->error().name(), ERL_NIF_LATIN1)
				) );
				delete db;
			}
		}
	}
	void DbOpenTask::SetDbFile(const char* file) {
		// strlcpy(_DbFile, file, MAX_PATH_LEN);
		for (int i = 0; i < MAX_PATH_LEN; i++) {
			_DbFile[i] = file[i];
			if ( file[i] == '\0' ) break;
		}
	}

}
