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
