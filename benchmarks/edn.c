
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

/*
 * MDH WCET BENCHMARK SUITE. File version $Id: edn.c,v 1.1 2005/11/11
 * 10:14:10 ael01 Exp $
 */

/************************************************************************
*	Simple vector multiply				*
************************************************************************/

/*
 * Changes: JG 2005/12/22: Inserted prototypes, changed type of main to int
 * etc. Added parenthesis in expressions in jpegdct. Removed unused variable
 * dx. Changed int to long to avoid problems when compiling to 16 bit target
 * Indented program.
 * JG 2006-01-27: Removed code in codebook
 */

#define N 100
#define ORDER 50

void            vec_mpy1(short y[], const short x[], short scaler, int tam);
long int        mac(const short *a, const short *b, long int sqr, long int *sum);
void            fir(const short array1[], const short coeff[], long int output[]);
void            fir_no_red_ld(const short x[], const short h[], long int y[]);
long int        latsynth(short b[], const short k[], long int n, long int f);
void            iir1(const short *coefs, const short *input, long int *optr, long int *state);
long int        codebook(long int mask, long int bitchanged, long int numbasis, long int codeword, long int g, const short *d, short ddim, short theta);
void            jpegdct(short *d, short *r);

void
vec_mpy1(short y[], const short x[], short scaler, int tam)
{
	long int        i;

	for (i = 0; i < tam; i++)
		y[i] += ((scaler * x[i]) >> 15);
}


/*****************************************************
*			Dot Product	      *
*****************************************************/
long int
mac(const short *a, const short *b, long int sqr, long int *sum)
{
	long int        i;
	long int        dotp = *sum;

	for (i = 0; i < 150; i++) {
		dotp += b[i] * a[i];
		sqr += b[i] * b[i];
	}

	*sum = dotp;
	return sqr;
}


/*****************************************************
*		FIR Filter		     *
*****************************************************/
void
fir(const short array1[], const short coeff[], long int output[])
{
	long int        i, j, sum;

	for (i = 0; i < N - ORDER; i++) {
		sum = 0;
		for (j = 0; j < ORDER; j++) {
			sum += array1[i + j] * coeff[j];
		}
		output[i] = sum >> 15;
	}
}

/****************************************************
*	FIR Filter with Redundant Load Elimination

By doing two outer loops simultaneously, you can potentially  reuse data (depending on the DSP architecture).
x and h  only  need to be loaded once, therefore reducing redundant loads.
This reduces memory bandwidth and power.
*****************************************************/
void
fir_no_red_ld(const short x[], const short h[], long int y[])
{
	long int        i, j;
	long int        sum0, sum1;
	short           x0, x1, h0, h1;
	for (j = 0; j < 100; j += 2) {
		sum0 = 0;
		sum1 = 0;
		x0 = x[j];
		for (i = 0; i < 32; i += 2) {
			x1 = x[j + i + 1];
			h0 = h[i];
			sum0 += x0 * h0;
			sum1 += x1 * h0;
			x0 = x[j + i + 2];
			h1 = h[i + 1];
			sum0 += x1 * h1;
			sum1 += x0 * h1;
		}
		y[j] = sum0 >> 15;
		y[j + 1] = sum1 >> 15;
	}
}

/*******************************************************
*	Lattice Synthesis	           *
* This function doesn't follow the typical DSP multiply two  vector operation, but it will point out the compiler's flexibility   ********************************************************/
long int
latsynth(short b[], const short k[], long int n, long int f)
{
	long int        i;

	f -= b[n - 1] * k[n - 1];
	for (i = n - 2; i >= 0; i--) {
		f -= b[i] * k[i];
		b[i + 1] = b[i] + ((k[i] * (f >> 16)) >> 16);
	}
	b[0] = f >> 16;
	return f;
}

/*****************************************************
*			IIR Filter		     *
*****************************************************/
void
iir1(const short *coefs, const short *input, long int *optr, long int *state)
{
	long int        x;
	long int        t;
	long int        n;

	x = input[0];
	for (n = 0; n < 50; n++) {
		t = x + ((coefs[2] * state[0] + coefs[3] * state[1]) >> 15);
		x = t + ((coefs[0] * state[0] + coefs[1] * state[1]) >> 15);
		state[1] = state[0];
		state[0] = t;
		coefs += 4;	/* point to next filter coefs  */
		state += 2;	/* point to next filter states */
	}
	*optr++ = x;
}

/*****************************************************
*	Vocoder Codebook Search 	     *
*****************************************************/
long int
codebook(long int mask, long int bitchanged, long int numbasis, long int codeword, long int g, const short *d, short ddim, short theta)
/*
 * dfm (mask=d  bitchanged=1 numbasis=17  codeword=e[0] , g=d, d=a, ddim=c,
 * theta =1
 */

{
	long int        j;
	//long int        tmpMask;
	//tmpMask = mask << 1;
	for (j = bitchanged + 1; j <= numbasis; j++) {



/*
 * The following code is removed since it gave a memory access exception.
 * It is OK since the return value does not control the flow.
 * The loop always iterates a fixed number of times independent of the loop body.

    if (theta == !(!(codeword & tmpMask)))
			g += *(d + bitchanged * ddim + j);
		else
			g -= *(d + bitchanged * ddim + j);
		tmpMask <<= 1;
*/
	}
	return g;
}


/*****************************************************
*		JPEG Discrete Cosine Transform 		     *
*****************************************************/
void
jpegdct(short *d, short *r)
{
	long int        t[12];
	short           i, j, k, m, n, p;
	for (k = 1, m = 0, n = 13, p = 8; k <= 8; k += 7, m += 3, n += 3, p -= 7, d -= 64) {
		for (i = 0; i < 8; i++, d += p) {
			for (j = 0; j < 4; j++) {
				t[j] = d[k * j] + d[k * (7 - j)];
				t[7 - j] = d[k * j] - d[k * (7 - j)];
			}
			t[8] = t[0] + t[3];
			t[9] = t[0] - t[3];
			t[10] = t[1] + t[2];
			t[11] = t[1] - t[2];
			d[0] = (t[8] + t[10]) >> m;
			d[4 * k] = (t[8] - t[10]) >> m;
			t[8] = (short) (t[11] + t[9]) * r[10];
			d[2 * k] = t[8] + (short) ((t[9] * r[9]) >> n);
			d[6 * k] = t[8] + (short) ((t[11] * r[11]) >> n);
			t[0] = (short) (t[4] + t[7]) * r[2];
			t[1] = (short) (t[5] + t[6]) * r[0];
			t[2] = t[4] + t[6];
			t[3] = t[5] + t[7];
			t[8] = (short) (t[2] + t[3]) * r[8];
			t[2] = (short) t[2] * r[1] + t[8];
			t[3] = (short) t[3] * r[3] + t[8];
			d[7 * k] = (short) (t[4] * r[4] + t[0] + t[2]) >> n;
			d[5 * k] = (short) (t[5] * r[6] + t[1] + t[3]) >> n;
			d[3 * k] = (short) (t[6] * r[5] + t[1] + t[2]) >> n;
			d[1 * k] = (short) (t[7] * r[7] + t[0] + t[3]) >> n;
		}
	}
}


void calculate(short a[], short b[], int tam){


	short           c = 0x3;
	long int        output[200];
	long int        d = 0xAAAA;
	int             e[1] = {0xEEEE};
	/*
	 * Declared as memory variable so it doesn't get optimized out
	 */

	vec_mpy1(a, b, c, tam);
	c = mac(a, b, (long int) c, (long int *) output);
	fir(a, b, output);
	fir_no_red_ld(a, b, output);
	d = latsynth(a, b, N, d);
	iir1(a, b, &output[100], output);
	e[0] = codebook(d, 1, 17, e[0], d, a, c, 1);
	jpegdct(a, b);

}



// function to save data in saida.csv
void save_data(uint64_t val_cycles, uint64_t val_instruction){

	FILE *arq;

	arq = fopen("saida_edn.csv", "a");

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

  //para variacao da entrada.
  //numero aleatorio entre 1 e 200 para variar o tamanho dos vetores enviados a vec_mpy1
  //responsavel por fazer o deslocamento de bits <<
//   srand(time(0));
//   int tam = 1 + (rand() % 199);
   int tam = 157;

  short _a[200] = {0x0000, 0x07ff, 0x0c00, 0x0800, 0x0200, 0xf800, 0xf300, 0x0400, 0x0000,
		0x07ff, 0x0c00, 0x0800, 0x0200, 0xf800, 0xf300, 0x0400, 0x0000, 0x07ff, 0x0c00,
		0x0800, 0x0200, 0xf800, 0xf300, 0x0400, 0x0000, 0x07ff, 0x0c00, 0x0800, 0x0200,
		0xf800, 0xf300, 0x0400, 0x0000, 0x07ff, 0x0c00, 0x0800, 0x0200, 0xf800, 0xf300,
		0x0400, 0x0000, 0x07ff, 0x0c00, 0x0800, 0x0200, 0xf800, 0xf300, 0x0400, 0x0000,
		0x07ff, 0x0c00, 0x0800, 0x0200, 0xf800, 0xf300, 0x0400, 0x0000, 0x07ff, 0x0c00,
		0x0800, 0x0200, 0xf800, 0xf300, 0x0400, 0x0000, 0x07ff, 0x0c00, 0x0800, 0x0200,
		0xf800, 0xf300, 0x0400, 0x0000, 0x07ff, 0x0c00, 0x0800, 0x0200, 0xf800, 0xf300,
		0x0400, 0x0000, 0x07ff, 0x0c00, 0x0800, 0x0200, 0xf800, 0xf300, 0x0400, 0x0000,
		0x07ff, 0x0c00, 0x0800, 0x0200, 0xf800, 0xf300, 0x0400, 0x0000, 0x07ff, 0x0c00,
		0x0800, 0x0200, 0xf800, 0xf300, 0x0400, 0x0000, 0x07ff, 0x0c00, 0x0800, 0x0200,
		0xf800, 0xf300, 0x0400, 0x0000, 0x07ff, 0x0c00, 0x0800, 0x0200, 0xf800, 0xf300,
		0x0400, 0x0000, 0x07ff, 0x0c00, 0x0800, 0x0200, 0xf800, 0xf300, 0x0400, 0x0000,
		0x07ff, 0x0c00, 0x0800, 0x0200, 0xf800, 0xf300, 0x0400, 0x0000, 0x07ff, 0x0c00,
		0x0800, 0x0200, 0xf800, 0xf300, 0x0400, 0x0000, 0x07ff, 0x0c00, 0x0800, 0x0200,
		0xf800, 0xf300, 0x0400, 0x0000, 0x07ff, 0x0c00, 0x0800, 0x0200, 0xf800, 0xf300,
		0x0400, 0x0000, 0x07ff, 0x0c00, 0x0800, 0x0200, 0xf800, 0xf300, 0x0400, 0x0000,
		0x07ff, 0x0c00, 0x0800, 0x0200, 0xf800, 0xf300, 0x0400, 0x0000, 0x07ff, 0x0c00,
		0x0800, 0x0200, 0xf800, 0xf300, 0x0400, 0x0000, 0x07ff, 0x0c00, 0x0800, 0x0200,
		0xf800, 0xf300, 0x0400, 0x0000, 0x07ff, 0x0c00, 0x0800, 0x0200, 0xf800, 0xf300, 0x0400};


  short _b[200] = {0x0c60, 0x0c40, 0x0c20, 0x0c00, 0xf600, 0xf400, 0xf200, 0xf000, 0x0c60,
		0x0c40, 0x0c20, 0x0c00, 0xf600, 0xf400, 0xf200, 0xf000, 0x0c60, 0x0c40, 0x0c20,
		0x0c00, 0xf600, 0xf400, 0xf200, 0xf000, 0x0c60, 0x0c40, 0x0c20, 0x0c00, 0xf600,
		0xf400, 0xf200, 0xf000, 0x0c60, 0x0c40, 0x0c20, 0x0c00, 0xf600, 0xf400, 0xf200,
		0xf000, 0x0c60, 0x0c40, 0x0c20, 0x0c00, 0xf600, 0xf400, 0xf200, 0xf000, 0x0c60,
		0x0c40, 0x0c20, 0x0c00, 0xf600, 0xf400, 0xf200, 0xf000, 0x0c60, 0x0c40, 0x0c20,
		0x0c00, 0xf600, 0xf400, 0xf200, 0xf000, 0x0c60, 0x0c40, 0x0c20, 0x0c00, 0xf600,
		0xf400, 0xf200, 0xf000, 0x0c60, 0x0c40, 0x0c20, 0x0c00, 0xf600, 0xf400, 0xf200,
		0xf000, 0x0c60, 0x0c40, 0x0c20, 0x0c00, 0xf600, 0xf400, 0xf200, 0xf000, 0x0c60,
		0x0c40, 0x0c20, 0x0c00, 0xf600, 0xf400, 0xf200, 0xf000, 0x0c60, 0x0c40, 0x0c20,
		0x0c00, 0xf600, 0xf400, 0xf200, 0xf000, 0x0c60, 0x0c40, 0x0c20, 0x0c00, 0xf600,
		0xf400, 0xf200, 0xf000, 0x0c60, 0x0c40, 0x0c20, 0x0c00, 0xf600, 0xf400, 0xf200,
		0xf000, 0x0c60, 0x0c40, 0x0c20, 0x0c00, 0xf600, 0xf400, 0xf200, 0xf000, 0x0c60,
		0x0c40, 0x0c20, 0x0c00, 0xf600, 0xf400, 0xf200, 0xf000, 0x0c60, 0x0c40, 0x0c20,
		0x0c00, 0xf600, 0xf400, 0xf200, 0xf000, 0x0c60, 0x0c40, 0x0c20, 0x0c00, 0xf600,
		0xf400, 0xf200, 0xf000, 0x0c60, 0x0c40, 0x0c20, 0x0c00, 0xf600, 0xf400, 0xf200,
		0xf000, 0x0c60, 0x0c40, 0x0c20, 0x0c00, 0xf600, 0xf400, 0xf200, 0xf000, 0x0c60,
		0x0c40, 0x0c20, 0x0c00, 0xf600, 0xf400, 0xf200, 0xf000, 0x0c60, 0x0c40, 0x0c20,
		0x0c00, 0xf600, 0xf400, 0xf200, 0xf000, 0x0c60, 0x0c40, 0x0c20, 0x0c00, 0xf600,
		0xf400, 0xf200, 0xf000, 0x0c60, 0x0c40, 0x0c20, 0x0c00, 0xf600, 0xf400, 0xf200, 0xf000
	  };


  short a[tam];
  short b[tam];

  for(i = 0; i<tam; i++){
   a[i] = _a[i];
   b[i] = _b[i];
  }


  //execution
  ioctl(fd_cycles, PERF_EVENT_IOC_RESET, PERF_IOC_FLAG_GROUP);
  ioctl(fd_cycles, PERF_EVENT_IOC_ENABLE, PERF_IOC_FLAG_GROUP);

  calculate(a,b,tam);

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

  close(fd_cycles);
  close(fd_instruction);

}

