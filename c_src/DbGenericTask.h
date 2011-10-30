#ifndef _DbGeneric_Task_h
#define _DbGeneric_Task_h

#include "NifAsyncTask.h"

namespace kyte {
	using kyotocabinet::PolyDB;

	class DbGenericTask : public NifAsyncTask {
	private:
		int _DbIdx;
	public:
		DbGenericTask();
		virtual ~DbGenericTask();
		
		void SetDbIdx(int dbIdx);

	protected:
		int GetDbIdx() const;
		PolyDB*& DB();
		bool EnsureDB();
	};
}

#endif // _DbGeneric_Task_h
