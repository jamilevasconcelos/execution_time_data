
#define _GNU_SOURCE
#include <sched.h>
#include <sys/wait.h>
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
#include <stdio.h>
#include <stdlib.h>


// mat1[R1][C1] and mat2[R2][C2]
#define R1 20 // number of rows in Matrix-1
#define C1 20 // number of columns in Matrix-1
#define R2 20 // number of rows in Matrix-2
#define C2 20 // number of columns in Matrix-2

// function to save data in saida.csv
void save_data(uint64_t val_cycles, uint64_t val_instruction){

	FILE *arq;

	arq = fopen("saida_matmult.csv", "a");

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

void mulMat(int mat1[][C1], int mat2[][C2])
{
	int rslt[R1][C2];

	//printf("Multiplication of given two matrices is:\n");

	for (int i = 0; i < R1; i++) {
		for (int j = 0; j < C2; j++) {
			rslt[i][j] = 0;

			for (int k = 0; k < R2; k++) {
				rslt[i][j] += mat1[i][k] * mat2[k][j];
			}

			//printf("%d\t", rslt[i][j]);
		}

//		printf("\n");
	}
}

// Driver code
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

{
	// R1 = 4, C1 = 4 and R2 = 4, C2 = 4 (Update these
	// values in MACROs)

  ioctl(fd_cycles, PERF_EVENT_IOC_RESET, PERF_IOC_FLAG_GROUP);
  ioctl(fd_cycles, PERF_EVENT_IOC_ENABLE, PERF_IOC_FLAG_GROUP);


int mat1[R1][C1] = {
	{3459, 1183, 7406, 4037, 7016, 4976, 3298, 2112, 3035, 5472, 2833, 3255, 3330, 2019, 3819, 2794, 4950, 6614, 6198, 2504},
	{6587, 3853, 5745, 3201, 1562, 4749, 2259, 7444, 1283, 1242, 2086, 7179, 2145, 5182, 6486, 7857, 7961, 568, 4069, 7469},
	{5298, 6928, 2004, 3420, 1586, 7338, 5420, 765, 1596, 7920, 1980, 6799, 2432, 6663, 5196, 3740, 984, 279, 6291, 5974},
	{4898, 5428, 5949, 1440, 348, 472, 1089, 2302, 289, 5596, 5904, 1317, 987, 715, 4135, 7727, 4481, 226, 4805, 942},
	{8026, 3444, 216, 4724, 1388, 4455, 5338, 798, 585, 6165, 3689, 4055, 1899, 802, 6434, 4187, 5979, 3879, 7006, 4963},
	{7791, 6774, 7909, 5405, 4649, 1109, 5916, 4291, 3921, 7889, 3576, 3576, 1307, 3462, 1882, 2727, 1218, 3252, 6704, 6223},
	{7860, 1254, 3937, 5191, 4152, 5352, 1143, 3124, 3925, 6437, 5333, 7991, 4566, 4570, 7572, 914, 524, 1463, 4336, 614},
	{6427, 6676, 7568, 2027, 3190, 6055, 6706, 4076, 5373, 6179, 4064, 197, 5599, 3274, 7153, 3570, 2568, 5034, 6157, 1493},
	{2044, 6049, 7439, 511, 6316, 3274, 1804, 4543, 2931, 1839, 1050, 5529, 933, 5123, 4419, 6524, 3815, 2936, 1203, 8017},
	{5144, 5591, 6658, 5416, 1711, 1276, 4156, 722, 5686, 5602, 7781, 1068, 8042, 7079, 2513, 4173, 8070, 3942, 6636, 5525},
	{779, 5983, 5006, 5890, 1324, 4443, 3211, 5371, 3379, 1078, 6110, 4928, 1832, 2051, 1592, 7626, 3342, 7106, 3219, 3404},
	{7203, 7314, 3354, 7505, 1243, 7431, 4437, 3273, 4579, 7086, 4170, 3559, 5727, 4223, 7587, 2312, 2736, 4442, 3529, 7893},
	{7870, 6976, 4782, 2817, 6266, 2564, 6291, 7428, 3032, 5739, 2580, 7023, 3482, 4572, 3259, 3832, 5479, 5919, 1905, 7165},
	{492, 4081, 6878, 4175, 7071, 1068, 7775, 3422, 7, 6189, 4034, 6865, 7561, 3275, 6237, 2156, 2656, 1587, 6355, 6877},
	{855, 6126, 762, 3894, 3868, 4076, 2578, 5300, 6424, 6067, 4885, 1972, 5972, 1149, 7158, 7939, 777, 1887, 742, 7617},
	{2721, 5961, 4282, 3374, 1780, 7274, 7614, 6603, 7859, 6246, 8048, 2356, 6278, 1537, 8055, 4492, 6718, 1461, 2783, 1855},
	{2867, 5564, 1855, 7272, 305, 1265, 6185, 6459, 164, 6150, 4659, 5422, 1332, 6370, 4189, 2616, 7260, 1914, 3564, 7707},
	{7430, 1567, 7256, 4089, 1847, 7549, 6718, 309, 6537, 5155, 4090, 193, 7754, 3765, 6202, 3066, 7400, 2539, 807, 6143},
	{4524, 6286, 4258, 520, 3215, 5308, 1173, 6890, 6170, 7286, 7735, 4900, 3507, 3382, 5389, 7652, 41, 3493, 2038, 6213},
	{5237, 5925, 7807, 2083, 3127, 3135, 4736, 2483, 3361, 582, 1578, 776, 6761, 4592, 2276, 6357, 7724, 3386, 203, 7997}
};

int mat2[R2][C2] = {
	{1712, 7759, 366, 5114, 356, 6946, 1720, 3114, 1179, 4604, 7628, 898, 5716, 1546, 4749, 2523, 3755, 2013, 2967, 1053},
	{1486, 4083, 2163, 7379, 3519, 1617, 8001, 1192, 5890, 3669, 7282, 3809, 5358, 6255, 1435, 4772, 4481, 4370, 6565, 2483},
	{6415, 3081, 816, 7322, 2458, 7742, 3967, 1856, 3946, 4772, 3341, 1917, 4469, 3596, 2306, 5924, 183, 2655, 1742, 3538},
	{2156, 793, 3906, 2784, 951, 4359, 30, 2497, 5043, 6068, 6708, 7530, 6180, 4688, 3763, 1733, 2736, 5856, 3560, 7874},
	{1610, 420, 4716, 7264, 7975, 247, 2940, 6760, 5989, 6928, 7884, 2903, 3506, 3825, 4258, 2569, 32, 4329, 3471, 1138},
	{5903, 4046, 5985, 8011, 5064, 1272, 2722, 7432, 2979, 1481, 2306, 46, 651, 4271, 6281, 4484, 670, 6143, 3239, 388},
	{2479, 4032, 1596, 2687, 7901, 3648, 618, 77, 3508, 5263, 7493, 6069, 6985, 5037, 6884, 4646, 969, 471, 7000, 1045},
	{974, 3887, 4893, 3225, 5336, 1245, 1700, 5122, 865, 98, 3887, 5253, 5547, 5127, 4450, 5499, 1745, 7593, 2968, 1836},
	{1747, 4051, 6761, 4555, 3908, 4183, 221, 8077, 4993, 7248, 7503, 1807, 414, 6256, 3071, 4378, 7638, 7469, 2834, 7776},
	{6928, 5998, 1389, 4241, 7876, 3941, 3039, 1569, 6561, 3537, 5905, 3176, 254, 811, 1270, 639, 2485, 1527, 3160, 4005},
	{6107, 2669, 4924, 4137, 4204, 6417, 985, 2924, 3611, 587, 3865, 379, 3613, 5306, 6566, 3780, 4966, 4213, 3607, 1660},
	{7786, 4440, 5441, 6743, 2387, 724, 4391, 3673, 2981, 3288, 3774, 5198, 3750, 4366, 6098, 766, 4873, 3410, 6971, 577},
	{7854, 3749, 2104, 6808, 3609, 3638, 7072, 7545, 5957, 456, 3493, 4757, 2195, 2175, 1248, 1069, 6839, 7888, 1183, 6991},
	{4721, 483, 3110, 1808, 3424, 434, 7460, 6803, 6971, 5411, 5954, 8077, 1918, 7013, 4236, 7654, 2728, 5214, 6615, 259},
	{6483, 7943, 5437, 5015, 7708, 6235, 4534, 2300, 4508, 5516, 6001, 3063, 5322, 5960, 7706, 1791, 4644, 6179, 652, 1126},
	{6326, 788, 5152, 3194, 3803, 4712, 3763, 1493, 2398, 158, 6740, 3423, 6501, 5927, 756, 3334, 7426, 329, 3032, 6358},
	{2770, 3443, 2638, 7857, 5106, 1681, 7078, 4989, 1480, 3327, 5608, 2417, 7013, 1436, 3100, 1940, 1979, 812, 643, 5094},
	{6454, 4823, 5724, 1739, 7835, 3228, 3395, 5894, 2083, 5122, 6634, 3328, 4928, 2752, 2602, 2664, 6944, 2348, 7518, 5365},
	{3695, 358, 130, 5785, 1842, 4234, 1312, 3436, 7736, 1809, 5593, 7212, 2165, 3664, 641, 3832, 5842, 3791, 225, 3133},
	{3634, 1382, 3578, 4050, 2169, 2247, 2465, 914, 6462, 6647, 5998, 5907, 1148, 4233, 819, 6230, 4903, 3191, 4608, 7753}
};



	mulMat(mat1, mat2);

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
}

