/**
 * This file is a part of Kyte released under the MIT licence.
 * See the LICENCE file for more information
 */

#include "ITask.h"

#include <stdio.h>

namespace RG {
	ITask::ITask() : _Name(NULL) {
		SetName("unnamed");
	}
	ITask::~ITask() {
		delete [] _Name;
	}
	void ITask::Run() {}
	
	void ITask::SetName(const char* name) {
		if (_Name) 
			delete [] _Name;
			
		int len = 0;
		char ch = 0x00;
		do {
			ch = name[len];
			len++;
		} while (ch);
		_Name = new char[len + 1];
		for (int i = 0; i < len; i++) {
			_Name[i] = name[i];
		}
		_Name[len] = 0x00;
	}
	const char* ITask::GetName() const {
		return _Name;
	}
	bool ITask::ToBeDisposedByWorker() const {
		return false;
	}
}
