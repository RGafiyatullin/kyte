
#include <assert.h>
#include <stdio.h>
#include <kcpolydb.h>

#include "kyte.h"
#include "TaskQueue.h"

#include "NifAsyncTask.h"
#include "DbOpenTask.h"
#include "DbCloseTask.h"
#include "DbSetTask.h"
#include "DbGetTask.h"
#include "DbRemoveTask.h"
#include "DbClearTask.h"
#include "DbCountTask.h"
#include "DbSizeTask.h"

extern "C" {
	#include <erl_nif.h>

	#define CHECK_THR_POOL(Idx) \
		if ( TaskQueues[Idx] == NULL ) { \
			return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "bad_pool_idx")); \
		}
	#define STD_TASK_BEGIN(TaskType) \
		RG::TaskQueue* tq = TaskQueues[thrPoolIdx]; \
		kyte::TaskType* task = new kyte::TaskType; \
		task->SetOpenDBs( OpenDatabases[thrPoolIdx] ); \
		task->SetReplyTo( argv[0], argv[1] );
	#define STD_TASK_END() \
		tq->AddTask(task);	

	static RG::TaskQueue* TaskQueues[MAX_THR_POOLS];

	using kyotocabinet::PolyDB;
	static PolyDB** OpenDatabases[MAX_THR_POOLS];

	static ERL_NIF_TERM kc_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
	{
		for (int i = 0; i < MAX_THR_POOLS; i++) {
			TaskQueues[i] = NULL;
			OpenDatabases[i] = new PolyDB * [MAX_OPEN_DBS];
			for (int j = 0; j < MAX_OPEN_DBS; j++) {
				OpenDatabases[i][j] = NULL;
			}
		}
		return enif_make_atom(env, "ok");
	}

	static ERL_NIF_TERM kc_create_thr_pool(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
	{
		assert( argc == 1 );
		int thrCnt = 0;
		assert( enif_get_int(env, argv[0], &thrCnt) == true );
		
		RG::TaskQueue* tq = new RG::TaskQueue(thrCnt);
		int poolPos = place_to_the_pool(tq, TaskQueues, MAX_THR_POOLS);
		if ( poolPos == -1 ) {
			delete tq;
			return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "busy"));
		}
		else {
			return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_int(env, poolPos));
		}
	}
	static ERL_NIF_TERM kc_destroy_thr_pool(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
	{
		assert( argc == 1 );
		int poolIdx;
		assert( enif_get_int(env, argv[0], &poolIdx) == true );
		
		CHECK_THR_POOL(poolIdx);
		
		RG::TaskQueue* tq = TaskQueues[poolIdx];
		PolyDB** openDBs = OpenDatabases[poolIdx];
		for (int i = 0; i < MAX_OPEN_DBS; i++) {
			if ( openDBs[i] != NULL ) {
				openDBs[i]->close();
				delete openDBs[i];
				openDBs[i] = NULL;
			}
		}
		
		tq->Shutdown();
		delete tq;
		TaskQueues[poolIdx] = NULL;

		return enif_make_atom(env, "ok");
	}

	static ERL_NIF_TERM kc_db_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
	{
		assert( argc == 4 );
		assert( enif_is_pid(env, argv[0]) == true );
		// assert( enif_is_ref(env, argv[1]) == true );

		int thrPoolIdx;
		char pathToDb[ MAX_PATH_LEN + 1 ];
		assert( enif_get_int(env, argv[2], &thrPoolIdx) == true );
		assert( thrPoolIdx >= 0 && thrPoolIdx < MAX_THR_POOLS );
		assert( enif_get_string(env, argv[3], pathToDb, MAX_PATH_LEN, ERL_NIF_LATIN1) > 0 );
		
		CHECK_THR_POOL(thrPoolIdx);
		STD_TASK_BEGIN(DbOpenTask);

		task->SetDbFile(pathToDb);

		STD_TASK_END();
		return enif_make_atom(env, "ok");
	}

	static ERL_NIF_TERM kc_db_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
	{
		assert( argc == 4 );
		assert( enif_is_pid(env, argv[0]) == true );
		// assert( enif_is_ref(env, argv[1]) == true );

		int thrPoolIdx;
		int dbIdx;
		assert( enif_get_int(env, argv[2], &thrPoolIdx) == true );
		assert( enif_get_int(env, argv[3], &dbIdx) == true );

		CHECK_THR_POOL(thrPoolIdx);
		STD_TASK_BEGIN(DbCloseTask);

		task->SetDbIdx(dbIdx);

		STD_TASK_END();
		return enif_make_atom(env, "ok");
	}

	static ERL_NIF_TERM kc_db_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
	{
		assert( argc == 4 );
		assert( enif_is_pid(env, argv[0]) == true );
		// assert( enif_is_ref(env, argv[1]) == true );

		int thrPoolIdx;
		int dbIdx;
		assert( enif_get_int(env, argv[2], &thrPoolIdx) == true );
		assert( enif_get_int(env, argv[3], &dbIdx) == true );

		CHECK_THR_POOL(thrPoolIdx);
		STD_TASK_BEGIN(DbSizeTask);

		task->SetDbIdx(dbIdx);

		STD_TASK_END();
		return enif_make_atom(env, "ok");
	}

	static ERL_NIF_TERM kc_db_count(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
	{
		assert( argc == 4 );
		assert( enif_is_pid(env, argv[0]) == true );
		// assert( enif_is_ref(env, argv[1]) == true );

		int thrPoolIdx;
		int dbIdx;
		assert( enif_get_int(env, argv[2], &thrPoolIdx) == true );
		assert( enif_get_int(env, argv[3], &dbIdx) == true );

		CHECK_THR_POOL(thrPoolIdx);
		STD_TASK_BEGIN(DbCountTask);

		task->SetDbIdx(dbIdx);

		STD_TASK_END();
		return enif_make_atom(env, "ok");
	}

	static ERL_NIF_TERM kc_db_clear(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
	{
		assert( argc == 4 );
		assert( enif_is_pid(env, argv[0]) == true );
		// assert( enif_is_ref(env, argv[1]) == true );

		int thrPoolIdx;
		int dbIdx;
		assert( enif_get_int(env, argv[2], &thrPoolIdx) == true );
		assert( enif_get_int(env, argv[3], &dbIdx) == true );

		CHECK_THR_POOL(thrPoolIdx);
		STD_TASK_BEGIN(DbClearTask);

		task->SetDbIdx(dbIdx);

		STD_TASK_END();
		return enif_make_atom(env, "ok");
	}

	static ERL_NIF_TERM kc_db_set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
	{
		assert( argc == 6 );
		assert( enif_is_pid(env, argv[0]) == true );
		// assert( enif_is_ref(env, argv[1]) == true );

		int thrPoolIdx;
		int dbIdx;
		assert( enif_get_int(env, argv[2], &thrPoolIdx) == true );
		assert( enif_get_int(env, argv[3], &dbIdx) == true );

		CHECK_THR_POOL(thrPoolIdx);
		STD_TASK_BEGIN(DbSetTask);

		task->SetDbIdx(dbIdx);
		task->SetKey(argv[4]);
		task->SetValue(argv[5]);

		STD_TASK_END();
		return enif_make_atom(env, "ok");
	}

	static ERL_NIF_TERM kc_db_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
	{
		assert( argc == 5 );
		assert( enif_is_pid(env, argv[0]) == true );
		// assert( enif_is_ref(env, argv[1]) == true );

		int thrPoolIdx;
		int dbIdx;
		assert( enif_get_int(env, argv[2], &thrPoolIdx) == true );
		assert( enif_get_int(env, argv[3], &dbIdx) == true );

		CHECK_THR_POOL(thrPoolIdx);
		STD_TASK_BEGIN(DbGetTask);

		task->SetDbIdx(dbIdx);
		task->SetKey(argv[4]);

		STD_TASK_END();
		return enif_make_atom(env, "ok");
	}

	static ERL_NIF_TERM kc_db_remove(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
	{
		assert( argc == 5 );
		assert( enif_is_pid(env, argv[0]) == true );
		// assert( enif_is_ref(env, argv[1]) == true );

		int thrPoolIdx;
		int dbIdx;
		assert( enif_get_int(env, argv[2], &thrPoolIdx) == true );
		assert( enif_get_int(env, argv[3], &dbIdx) == true );

		CHECK_THR_POOL(thrPoolIdx);
		STD_TASK_BEGIN(DbRemoveTask);

		task->SetDbIdx(dbIdx);
		task->SetKey(argv[4]);

		STD_TASK_END();
		return enif_make_atom(env, "ok");
	}

	static ErlNifFunc nif_funcs[] =
	{
		{"init_nif", 0, kc_init_nif},
		
		{"create_thr_pool", 1, kc_create_thr_pool},
		{"destroy_thr_pool", 1, kc_destroy_thr_pool},

		{"db_open", 4, kc_db_open},
		{"db_close", 4, kc_db_close},

		{"db_set", 6, kc_db_set},
		{"db_get", 5, kc_db_get},
		{"db_remove", 5, kc_db_remove},

		{"db_clear", 4, kc_db_clear},
		{"db_count", 4, kc_db_count},
		{"db_size", 4, kc_db_size}
	};

	ERL_NIF_INIT( kyte_nifs,
				nif_funcs,
				NULL, NULL,
				NULL,NULL )


}