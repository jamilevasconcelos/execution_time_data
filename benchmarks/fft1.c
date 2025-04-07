
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

/*************************************************************************/
/*                                                                       */
/*   SNU-RT Benchmark Suite for Worst Case Timing Analysis               */
/*   =====================================================               */
/*                              Collected and Modified by S.-S. Lim      */
/*                                           sslim@archi.snu.ac.kr       */
/*                                         Real-Time Research Group      */
/*                                        Seoul National University      */
/*                                                                       */
/*                                                                       */
/*        < Features > - restrictions for our experimental environment   */
/*                                                                       */
/*          1. Completely structured.                                    */
/*               - There are no unconditional jumps.                     */
/*               - There are no exit from loop bodies.                   */
/*                 (There are no 'break' or 'return' in loop bodies)     */
/*          2. No 'switch' statements.                                   */
/*          3. No 'do..while' statements.                                */
/*          4. Expressions are restricted.                               */
/*               - There are no multiple expressions joined by 'or',     */
/*                'and' operations.                                      */
/*          5. No library calls.                                         */
/*               - All the functions needed are implemented in the       */
/*                 source file.                                          */
/*                                                                       */
/*                                                                       */
/*************************************************************************/
/*                                                                       */
/*  FILE: fft1.c                                                         */
/*  SOURCE : Turbo C Programming for Engineering by Hyun Soon Ahn        */
/*                                                                       */
/*  DESCRIPTION :                                                        */
/*                                                                       */
/*     FFT using Cooly-Turkey algorithm.                                 */
/*     There are two inputs, ar[] and ai[]. ar[] is real number parts    */
/*     of input array and the ai[] is imaginary number parts of input.   */
/*     The function fft1 process FFT or inverse FFT according to the    .*/
/*     parameter flag. (FFT with flag=0, inverse FFT with flag=1).       */
/*                                                                       */
/*                                                                       */
/*  REMARK :                                                             */
/*                                                                       */
/*  EXECUTION TIME :                                                     */
/*                                                                       */
/*                                                                       */
/*************************************************************************/


#define PI 3.14159
#define M_PI 3.14159

double ar[8];
double ai[8] = {0.,  };

int fft1(int n, int flag);


static double fabs(double n)
{
  double f;

  if (n >= 0) f = n;
  else f = -n;
  return f;
}

static double log(double n)
{
  return(4.5);
}


static double sin(rad)
double rad;
{
  double app;

  double diff;
  int inc = 1;

  while (rad > 2*PI)
	rad -= 2*PI;
  while (rad < -2*PI)
    rad += 2*PI;
  app = diff = rad;
   diff = (diff * (-(rad*rad))) /
      ((2.0 * inc) * (2.0 * inc + 1.0));
    app = app + diff;
    inc++;
  while(fabs(diff) >= 0.00001) {
    diff = (diff * (-(rad*rad))) /
      ((2.0 * inc) * (2.0 * inc + 1.0));
    app = app + diff;
    inc++;
  }

  return(app);
}


static double cos(double rad)
{
  double sin();

  return (sin (PI / 2.0 - rad));
}


// function to save data in saida.csv
void save_data(uint64_t val_cycles, uint64_t val_instruction){

	FILE *arq;

	arq = fopen("saida_fft1.csv", "a");

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
  } values[];
};



/* The main function */
int main(int argc, char* argv[]) {

   struct perf_event_attr pea;
   int fd_cycles, fd_instruction;
   uint64_t id_cycles, id_instruction;
   uint64_t val_cycles, val_instruction;

  char buf[4096];
  struct read_format* rf = (struct read_format*) buf;
  int i;

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
  fd_cycles = syscall(__NR_perf_event_open, &pea, 0, -1, -1, 0);
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
  fd_instruction = syscall(__NR_perf_event_open, &pea, 0, -1, fd_cycles, 0);
  ioctl(fd_instruction, PERF_EVENT_IOC_ID, &id_instruction);

  //srand(time(0));
  //int number = rand() % 492;
  int number  = 452;

  //execution
  ioctl(fd_cycles, PERF_EVENT_IOC_RESET, PERF_IOC_FLAG_GROUP);
  ioctl(fd_cycles, PERF_EVENT_IOC_ENABLE, PERF_IOC_FLAG_GROUP);

	int p, n = number, flag, chkerr;

	/* ar  */
	for(p = 0; p < n; p++)
	  ar[p] = cos(2*M_PI*p/n);

	/* forward fft */
	flag = 0;
	chkerr = fft1(n, flag);

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
    printf("EV1: %"PRIu64"\n",val_ev1);
    printf("EV2: %"PRIu64"\n",val_ev2);
    printf("EV3: %"PRIu64"\n",val_ev3);
    printf("EV4: %"PRIu64"\n",val_ev4);
	printf("EV5: %"PRIu64"\n",val_ev5);*/



  close(fd_cycles);
  close(fd_instruction);

}

int fft1(int n, int flag)
{

	 int i, j, k, it, xp, xp2, j1, j2, iter;
	 double sign, w, wr, wi, dr1, dr2, di1, di2, tr, ti, arg;

	 if(n < 2) return(999);
	 iter = log((double)n)/log(2.0);
	 j = 1;
#ifdef DEBUG
	printf("iter=%d\n",iter);
#endif
	 for(i = 0; i < iter; i++)
	   j *= 2;
	 if(fabs(n-j) > 1.0e-6)
	   return(1);

	 /*  Main FFT Loops  */
	 sign = ((flag == 1) ? 1.0 : -1.0);
	 xp2 = n;
	 for(it = 0; it < iter; it++)
	 {
			 xp = xp2;
			 xp2 /= 2;
			 w = PI / xp2;
#ifdef DEBUG
	printf("xp2=%d\n",xp2);
#endif
			 for(k = 0; k < xp2; k++)
			 {
					 arg = k * w;
					 wr = cos(arg);
					 wi = sign * sin(arg);
					 i = k - xp;
					 for(j = xp; j <= n; j += xp)
					 {
							 j1 = j + i;
							 j2 = j1 + xp2;
							 dr1 = ar[j1];
							 dr2 = ar[j2];
							 di1 = ai[j1];
							 di2 = ai[j2];
							 tr = dr1 - dr2;
							 ti = di1 - di2;
							 ar[j1] = dr1 + dr2;
							 ai[j1] = di1 + di2;
							 ar[j2] = tr * wr - ti * wi;
							 ai[j2] = ti * wr + tr * wi;
					 }
			 }
	 }

	 /*  Digit Reverse Counter  */

	 j1 = n / 2;
	 j2 = n - 1;
	 j = 1;
#ifdef DEBUG
	printf("j2=%d\n",j2);
#endif
	 for(i = 1; i <= j2; i++)
	 {
			 if(i < j)
			 {
					tr = ar[j-1];
					ti = ai[j-1];
					ar[j-1] = ar[i-1];
					ai[j-1] = ai[i-1];
					ar[i-1] = tr;
					ai[i-1] = ti;
			 }
			 k = j1;
			 while(k < j)
			 {
					j -= k;
					k /= 2;
			 }
			 j += k;
	 }
	 if(flag == 0) return(0);
	 w = n;
	 for(i = 0; i < n; i++)
	 {
			 ar[i] /= w;
			 ai[i] /= w;
	 }
	 return(0);
}




