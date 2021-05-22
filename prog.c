//// This is a simple program meant to be hacked using ptrace. The flag can be found within the RIP register.

#include <stdio.h>

int flag = 0xABCD;
unsigned long long x = 0;

int main () {
	while (flag == 0xABCD) {
		++x;
	}
	printf("Modified flag variable! Flag now: %x\n", flag);
	return 0;
}
