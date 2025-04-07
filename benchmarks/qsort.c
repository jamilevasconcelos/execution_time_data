#define SWAP(a,b) temp=(a);(a)=(b);(b)=temp;
#define M 7
#define NSTACK 50

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

// Vetor com input fixo
int arr[1000] = {121, 173, 555, 553, 456, 69, 924, 494, 585, 590, 135, 150, 688, 422, 186, 383, 839, 114, 559, 915, 472, 922, 360, 352, 312, 489, 680, 825, 800, 796, 665, 845, 523, 122, 332, 168, 337, 553, 121, 487, 449, 790, 806, 175, 81, 142, 939, 564, 656, 873, 423, 209, 743, 464, 354, 696, 430, 957, 607, 445, 986, 978, 883, 513, 183, 191, 519, 134, 261, 640, 690, 145, 602, 680, 632, 407, 30, 70, 58, 65, 618, 304, 479, 75, 197, 550, 797, 978, 876, 594, 282, 195, 786, 155, 464, 730, 665, 568, 145, 320, 583, 493, 231, 61, 191, 322, 898, 615, 712, 193, 218, 123, 914, 829, 570, 373, 298, 944, 423, 887, 587, 187, 688, 661, 761, 372, 795, 894, 209, 54, 259, 55, 588, 430, 533, 817, 932, 180, 66, 185, 790, 962, 375, 642, 337, 126, 321, 114, 132, 363, 904, 743, 143, 110, 881, 867, 897, 73, 779, 146, 640, 857, 979, 485, 242, 854, 675, 357, 799, 914, 484, 807, 550, 288, 395, 247, 708, 200, 882, 780, 276, 109, 344, 929, 350, 402, 313, 35, 205, 399, 518, 831, 232, 249, 726, 112, 38, 208, 778, 76, 417, 284, 761, 362, 453, 54, 440, 372, 689, 521, 86, 460, 968, 790, 945, 580, 561, 753, 413, 198, 596, 782, 802, 466, 21, 427, 865, 664, 350, 286, 319, 845, 184, 928, 370, 33, 564, 86, 675, 994, 573, 971, 316, 484, 633, 843, 581, 713, 566, 83, 266, 898, 327, 240, 272, 65, 517, 156, 862, 736, 738, 678, 717, 142, 361, 583, 73, 575, 318, 279, 762, 732, 934, 455, 463, 262, 729, 207, 770, 673, 888, 848, 591, 684, 38, 615, 374, 783, 941, 918, 868, 359, 135, 645, 619, 111, 79, 817, 472, 814, 821, 399, 973, 834, 797, 579, 622, 354, 449, 663, 281, 294, 223, 179, 481, 414, 616, 113, 252, 226, 263, 843, 909, 360, 94, 620, 541, 340, 862, 609, 272, 333, 956, 309, 389, 241, 952, 156, 919, 641, 793, 392, 629, 634, 786, 593, 398, 64, 275, 611, 84, 341, 315, 287, 471, 449, 342, 620, 407, 18, 238, 254, 467, 477, 337, 113, 930, 660, 802, 249, 628, 557, 338, 426, 967, 594, 328, 981, 310, 201, 920, 875, 498, 124, 720, 731, 938, 956, 844, 837, 976, 27, 135, 944, 61, 905, 491, 833, 381, 382, 800, 743, 179, 619, 575, 879, 28, 879, 123, 988, 792, 95, 558, 583, 329, 468, 354, 248, 743, 750, 291, 74, 815, 532, 738, 349, 141, 756, 242, 844, 420, 892, 257, 852, 318, 420, 184, 102, 402, 729, 857, 702, 645, 677, 167, 84, 1, 604, 89, 542, 350, 288, 417, 251, 387, 965, 485, 34, 463, 256, 45, 276, 713, 452, 733, 685, 983, 80, 413, 533, 939, 305, 99, 20, 851, 956, 19, 286, 778, 355, 451, 877, 686, 155, 239, 219, 772, 12, 531, 37, 86, 130, 485, 492, 910, 79, 305, 451, 283, 521, 772, 505, 240, 444, 154, 630, 176, 226, 29, 223, 839, 265, 681, 143, 821, 716, 908, 677, 938, 396, 922, 44, 535, 120, 968, 697, 987, 511, 541, 92, 488, 589, 782, 674, 440, 938, 88, 106, 385, 48, 255, 535, 776, 203, 221, 5, 729, 168, 746, 110, 953, 238, 707, 749, 936, 829, 865, 565, 626, 479, 829, 673, 263, 702, 753, 985, 597, 34, 12, 812, 885, 517, 71, 856, 792, 750, 162, 559, 75, 380, 161, 988, 458, 808, 604, 931, 146, 584, 530, 260, 31, 860, 63, 211, 669, 794, 799, 354, 712, 265, 615, 963, 651, 900, 350, 196, 402, 403, 283, 528, 798, 877, 525, 962, 709, 393, 742, 537, 47, 987, 199, 287, 634, 138, 256, 554, 653, 493, 711, 682, 269, 650, 448, 222, 568, 730, 19, 927, 688, 27, 49, 953, 283, 848, 793, 664, 462, 335, 800, 648, 899, 60, 585, 951, 406, 471, 586, 253, 199, 777, 309, 618, 810, 625, 858, 448, 36, 888, 99, 185, 290, 434, 596, 255, 295, 999, 164, 922, 477, 624, 722, 189, 92, 8, 73, 613, 855, 32, 755, 756, 68, 34, 343, 727, 414, 507, 646, 943, 770, 932, 773, 869, 555, 80, 633, 688, 123, 230, 682, 864, 348, 615, 634, 197, 701, 162, 880, 154, 804, 134, 249, 401, 768, 34, 264, 595, 645, 745, 471, 351, 547, 262, 561, 463, 592, 304, 500, 389, 381, 91, 539, 57, 778, 488, 323, 34, 168, 861, 415, 691, 49, 590, 341, 432, 21, 23, 891, 980, 844, 107, 27, 478, 383, 173, 920, 418, 307, 233, 654, 671, 120, 540, 810, 531, 806, 615, 824, 689, 589, 510, 686, 962, 311, 303, 102, 627, 677, 116, 12, 371, 673, 674, 780, 761, 119, 670, 266, 140, 242, 890, 142, 827, 140, 331, 154, 558, 48, 126, 18, 187, 287, 368, 258, 883, 481, 568, 500, 295, 103, 233, 628, 317, 244, 723, 684, 847, 82, 663, 108, 691, 640, 567, 934, 265, 938, 709, 939, 232, 958, 465, 708, 36, 768, 745, 899, 839, 632, 20, 582, 374, 755, 975, 935, 335, 710, 318, 331, 402, 741, 481, 473, 782, 936, 183, 263, 177, 395, 534, 729, 176, 989, 670, 17, 697, 205, 77, 783, 18, 32, 480, 117, 679, 220, 790, 146, 517, 526, 555, 925, 278, 329, 973, 678, 488, 653, 789, 713, 303, 506, 205, 684, 467, 956, 637, 929, 241, 926, 964, 257, 685, 708, 254, 846, 171, 960, 25, 159, 116, 527, 401, 789, 388, 920, 122, 756, 608, 308, 56, 818, 924, 224, 214, 76, 575, 872, 111, 431, 759, 444, 946, 904, 22, 350, 587, 298, 797, 789, 239, 385, 38, 79, 24, 712, 311, 424, 507, 843, 307, 838, 163, 559, 914, 249, 369, 650, 900, 688, 611, 498, 64, 24, 978, 761, 399, 823, 481, 815, 885, 862, 798, 499, 32, 480, 428, 551, 248, 769, 584, 276, 839, 330, 314, 319, 20, 179, 637, 514, 877, 823, 551};
int numero = 666;

void sort(int arr[], unsigned long n);
int istack[100];

// function to save data in saida_qsort.csv
void save_data(uint64_t val_cycles, uint64_t val_instruction){

	FILE *arq;

	arq = fopen("saida_qsort.csv", "a");

	if(arq == NULL){
		printf("ERROR OPEN FILE");
	}

	fprintf(arq, "%"PRIu64";%"PRIu64" \n", val_cycles,val_instruction);
	fclose(arq);


}

// function for to save input
void saveInput(int arr[], int arr_size){

	FILE *arq2;
	int i = 0;

	arq2 = fopen("input.txt", "a");

	if(arq2 == NULL){
		printf("ERROR OPEN FILE");
	}


	for(i = 0; i< arr_size; i++){
		fprintf(arq2," %i ", arr[i]);
	}

	fprintf(arq2," \n");
	fclose(arq2);

}


void sort(int arr[], unsigned long n){
	unsigned long   i, ir = n, j, k, l = 1;
	int jstack = 0;
	float a, temp;

	while (1) {
		if (ir - l < M) {
			for (j = l + 1; j <= ir; j++) {
				a = arr[j];
				for (i = j - 1; i >= l; i--) {
					if (arr[i] <= a)
						break;
					arr[i + 1] = arr[i];
				}
				arr[i + 1] = a;
			}
			if (jstack == 0)
				break;
			ir = istack[jstack--];
			l = istack[jstack--];
		} else {
			k = (l + ir) >> 1;
			SWAP(arr[k], arr[l + 1])
				if (arr[l] > arr[ir]) {
				SWAP(arr[l], arr[ir])
			}
			if (arr[l + 1] > arr[ir]) {
				SWAP(arr[l + 1], arr[ir])
			}
			if (arr[l] > arr[l + 1]) {
				SWAP(arr[l], arr[l + 1])
			}
			i = l + 1;
			j = ir;
			a = arr[l + 1];
			for (;;) {
				i++;
				while (arr[i] < a)
					i++;
				j--;
				while (arr[j] > a)
					j--;
				if (j < i)
					break;
				SWAP(arr[i], arr[j]);
			}
			arr[l + 1] = arr[j];
			arr[j] = a;
			jstack += 2;

			if (ir - i + 1 >= j - l) {
				istack[jstack] = ir;
				istack[jstack - 1] = i;
				ir = j - 1;
			} else {
				istack[jstack] = j - 1;
				istack[jstack - 1] = l;
				l = i;
			}
		}
	}
}

struct read_format {
  uint64_t nr;
  struct {
    uint64_t value;
    uint64_t id;
  } values[];
};


int main( int argc, char ** argv){

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

  int arr_size = sizeof(arr)/sizeof(arr[0]);

  //execution
  ioctl(fd_cycles, PERF_EVENT_IOC_RESET, PERF_IOC_FLAG_GROUP);
  ioctl(fd_cycles, PERF_EVENT_IOC_ENABLE, PERF_IOC_FLAG_GROUP);

    sort(arr,arr_size);

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


