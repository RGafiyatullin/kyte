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

#include "Mutex.h"

#include <stdio.h>

namespace RG {
	Mutex::Mutex() {
		pthread_mutexattr_t attr;
		pthread_mutexattr_init(&attr);
		pthread_mutex_init(&_Mutex, &attr);
		pthread_mutexattr_destroy(&attr);
	}
	Mutex::~Mutex() {
		pthread_mutex_destroy(&_Mutex);
	}
	
	void Mutex::Lock() {
		pthread_mutex_lock(&_Mutex);
	}
	void Mutex::Unlock() {
		pthread_mutex_unlock(&_Mutex);
	}
}
