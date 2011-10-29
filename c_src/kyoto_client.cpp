
#include <assert.h>
#include <stdio.h>
#include <kcpolydb.h>

#include "kyoto_client.h"
#include "TaskQueue.h"

#include "NifAsyncTask.h"
#include "DbOpenTask.h"

extern "C" {
	#include <erl_nif.h>

	static RG::TaskQueue* TaskQueues[MAX_THR_POOLS];

	using kyotocabinet::PolyDB;
	static PolyDB** OpenDatabases[MAX_THR_POOLS];

	static ERL_NIF_TERM kc_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
	{
		for (int i = 0; i < MAX_THR_POOLS; i++) {
			TaskQueues[i] = NULL;
			OpenDatabases[i] = new PolyDB * [MAX_OPEN_DBS];
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
		/*
		for (int i = 0; i < MAX_THR_POOLS; i++) {
			if ( TaskQueues[i] == NULL ) {
				RG::TaskQueue* tq = new RG::TaskQueue(thrCnt);
				TaskQueues[i] = tq;
				return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_int(env, i));
			}
		}
		
		return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "busy"));
		*/
	}

	static ERL_NIF_TERM kc_db_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
	{
		assert( argc == 4 );
		int thrPoolIdx;
		char pathToDb[ MAX_PATH_LEN + 1 ];
		assert( enif_get_int(env, argv[0], &thrPoolIdx) == true );
		assert( thrPoolIdx >= 0 && thrPoolIdx < MAX_THR_POOLS );
		assert( enif_get_string(env, argv[1], pathToDb, MAX_PATH_LEN, ERL_NIF_LATIN1) > 0 );

		assert( enif_is_pid(env, argv[2]) == true );
		assert( enif_is_ref(env, argv[3]) == true );

		if ( TaskQueues[thrPoolIdx] == NULL ) {
			return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "bad_pool_idx"));
		}

		RG::TaskQueue* tq = TaskQueues[thrPoolIdx];

		kyoto_client::DbOpenTask* task = new kyoto_client::DbOpenTask;
		task->SetOpenDBs( OpenDatabases[thrPoolIdx] );
		task->SetReplyTo(argv[2], argv[3]);
		task->SetDbFile(pathToDb);

		tq->AddTask(task);

		return enif_make_atom(env, "ok");
	}

	static ErlNifFunc nif_funcs[] =
	{
		{"init_nif", 0, kc_init_nif},
		{"create_thr_pool", 1, kc_create_thr_pool},
		{"db_open", 4, kc_db_open}
	};

	ERL_NIF_INIT(kyoto_nifs,nif_funcs,NULL,NULL,NULL,NULL)
}