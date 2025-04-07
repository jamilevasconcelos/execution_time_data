#define _GNU_SOURCE
#include <sched.h>
#include <sys/wait.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <string.h>
#include <sys/ioctl.h>
#include <linux/perf_event.h>
#include <linux/hw_breakpoint.h>
#include <asm/unistd.h>
#include <errno.h>
#include <stdint.h>
#include <inttypes.h>
#include <time.h>


// malardalen benchmark
float my_fabs (float x);
float my_sqrt (float val);

float my_fabs (float x)
{
	if (x < 0)
		return -x;
	else
		return x;
}

float my_sqrt (float val)
{
	float x = val / 10;

	float dx;

	double diff;
	double min_tol = 0.00001;

	int i, flag;

	flag = 0;
	if (val == 0)
		x = 0;
	else {
		for (i = 1; i < 20 ; i++) {
			if (!flag) {
				dx = (val - (x * x)) / (2.0 * x);
				x = x + dx;
				diff = val - (x * x);
				if (my_fabs(diff) <= min_tol)
					flag = 1;
			} else
				x = x;
		}
	}
	return (x);
}

// function to save data in saida.csv
void save_data(uint64_t val_cycles, uint64_t val_instruction){

	FILE *arq;

	arq = fopen("saida_sqrt.csv", "a");

	if(arq == NULL){
		printf("ERROR OPEN FILE");
	}

	fprintf(arq, "%"PRIu64";%"PRIu64" \n", val_cycles,val_instruction);
	fclose(arq);


}


struct read_format {
  uint64_t nr;
  struct {
    uint64_t value;
    uint64_t id;
  } values[7];
};

int main( int argc, char ** argv){

   struct perf_event_attr pea;
   int fd_cycles, fd_instruction;
   uint64_t id_cycles, id_instruction;
   uint64_t val_cycles, val_instruction;

  char buf[4096];
  struct read_format* rf = (struct read_format*) buf;
  int i;
  int cpu_id = -1;
  cpu_set_t set;

  CPU_ZERO(&set);
  CPU_SET(0,&set);
  sched_setaffinity(getpid(), sizeof(set), &set);

  // cycles
  memset(&pea, 0, sizeof(struct perf_event_attr));
  pea.type = PERF_TYPE_HARDWARE;
  pea.size = sizeof(struct perf_event_attr);
  pea.config = PERF_COUNT_HW_CPU_CYCLES;
  pea.disabled = 1;
  pea.exclude_kernel = 1;
  pea.exclude_hv = 1;
  pea.read_format = PERF_FORMAT_GROUP | PERF_FORMAT_ID;
  fd_cycles = syscall(__NR_perf_event_open, &pea, 0, cpu_id, -1, 0);
  ioctl(fd_cycles, PERF_EVENT_IOC_ID, &id_cycles);

  // instruction
  memset(&pea, 0, sizeof(struct perf_event_attr));
  pea.type = PERF_TYPE_HARDWARE;
  pea.size = sizeof(struct perf_event_attr);
  pea.config = PERF_COUNT_HW_INSTRUCTIONS;
  pea.disabled = 1;
  pea.exclude_kernel = 1;
  pea.exclude_hv = 1;
  pea.read_format = PERF_FORMAT_GROUP | PERF_FORMAT_ID;
  fd_instruction = syscall(__NR_perf_event_open, &pea, 0, cpu_id, fd_cycles, 0);
  ioctl(fd_instruction, PERF_EVENT_IOC_ID, &id_instruction);

  // srand(time(0));
  // int numero = rand() % 10000;

  int numero = 6666;

  //execution
  ioctl(fd_cycles, PERF_EVENT_IOC_RESET, PERF_IOC_FLAG_GROUP);
  ioctl(fd_cycles, PERF_EVENT_IOC_ENABLE, PERF_IOC_FLAG_GROUP);

  my_sqrt(numero);

  ioctl(fd_cycles, PERF_EVENT_IOC_DISABLE, PERF_IOC_FLAG_GROUP);


  read(fd_cycles, buf, sizeof(buf));
  for (i = 0; i < rf->nr; i++) {
    if (rf->values[i].id == id_cycles) {
      val_cycles = rf->values[i].value;
    } else if (rf->values[i].id == id_instruction) {
      val_instruction = rf->values[i].value;
    } 
  }

   save_data(val_cycles, val_instruction);

    /*printf("CYCLES: %"PRIu64"\n",val_cycles);
    printf("INSTRUCTION: %"PRIu64"\n",val_instruction);
    printf("CACHE BRANCH MISS: %"PRIu64"\n",val_ev1);
    printf("CACHE L1D MISS: %"PRIu64"\n",val_ev2);
    printf("CACHE MEMO ACCESS: %"PRIu64"\n",val_ev3);
    printf("CACHE L1I MISS: %"PRIu64"\n",val_ev4);
    printf("CACHE LL MISS: %"PRIu64"\n",val_ev5);*/

  close(fd_cycles);
  close(fd_instruction);
 
}


