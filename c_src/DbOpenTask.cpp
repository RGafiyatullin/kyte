/**

Copyright (C) 2011 Roman Gafiyatullin <romko.goofique@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

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
			if ( db->open(_DbFile, PolyDB::OCREATE | PolyDB::OWRITER) ) {
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
