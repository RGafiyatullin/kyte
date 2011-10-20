#include <stdio.h>

#include <ITask.h>
#include <TaskQueue.h>

#define ONE_SECOND 1

class MyTask : public RG::ITask {
public:
	MyTask() {}
	virtual~MyTask() {}

	void Run() {
		for (int i = 0; i < 1; i++) {
			sleep(ONE_SECOND);
			printf("Hello! I am task '%s' [%i]\n", GetName(), i);
		}
	}
};

int main(int argc, char** argv)
{
	MyTask tasks[100];

	RG::TaskQueue tq(4);
	for (int i = 0; i < 100; i++) {
		char buff[64];
		sprintf(buff, "task #%d", i);
		tasks[i].SetName(buff);
		tq.AddTask(&tasks[i]);
		if (i % 2 == 0) {
		    sleep(ONE_SECOND);
		}
	}
	
	char ch;
	scanf("%c", &ch);
	return 0;
}

