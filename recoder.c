#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CHUNK_SIZE 4096
#define NELEMS(x)  (sizeof(x) / sizeof((x)[0]))

const int STORE_OPCODES[] = { 0x8d, 0x9d, 0x99, 0x8e, 0x8c };

// TODO This should be a CLI option
const short BASE_ADDR = 0xc000;

int compare(const void *a, const void *b) {
  return (*(int*)a - *(int*)b);
}

int main(int argc, char *argv[]) {
    if (argc != 3) {
        fprintf(stderr, "Usage: %s SIDFILE NEWSIDFILE\n", argv[0]);
        return 1;
    }

    const char* src_path = argv[1];
    const char* dst_path = argv[2];

    FILE* src = fopen(src_path, "rb");
    if (!src) {
        fprintf(stderr, "Failed to open %s\n", src_path);
        return 2;
    }

    FILE* dst = fopen(dst_path, "wb");
    if (!dst) {
        fprintf(stderr, "Failed to open %s\n", dst_path);
        return 2;
    }

    // TODO Make sure its a valid SID file

    // TODO Get offsetData from header
    //fseek(src, 0x7c, SEEK_SET);

    char src_buf[CHUNK_SIZE] = "";
    //char dst_buf[CHUNK_SIZE] = "";

    int stores_pos[CHUNK_SIZE / 3] = {};
    int stores = 0;

    unsigned int global_pos = 0;
    size_t res = fread(src_buf, 1, CHUNK_SIZE, src);
    while (res > 0) {
        src_buf[res] = 0;
        //fwrite(src_buf, 1, res, dst);

        // Search for Stores on SID registers and save their locations on
        // stores_pos array.
        for (int i = 0; i < 5; i++) {
            const char* ptr = memchr(src_buf, STORE_OPCODES[i], res);

            while (ptr) {
                int op_hi = ptr[2] & 0xff;
                int op_lo = ptr[1] & 0xff;

                if (op_hi == 0xd4 && op_lo >= 0 && op_lo < 0x20) {
                    int opcode  = ptr[0] & 0xff;
                    int operand = (op_lo | op_hi << 8) & 0xffff;
                    int pos = ptr - src_buf;

                    stores_pos[stores++] = pos;

                    printf("Found write ($%x) to SID register $%x at $%x\n", opcode, operand, global_pos + pos);
                }
                ptr = memchr(ptr + 3, STORE_OPCODES[i], res);
            }
        }

        // Sort stores_pos array
        qsort(stores_pos, stores, sizeof(int), compare);

        // For each Store, append extra Store
        int old_pos = 0;
        for (int i = 0; i < stores; i++) {
            fwrite(src_buf + old_pos, 1, stores_pos[i] - old_pos, dst);

            char* ptr = src_buf + stores_pos[i];
            int opcode  = ptr[0] & 0xff;
            int op_lo  = ptr[1] & 0xff;

            char extra_store[3] = {
                opcode,
                BASE_ADDR + op_lo,
                (BASE_ADDR >> 8)
            };
            fwrite(extra_store, 1, 3, dst);

            old_pos = stores_pos[i];
        }
        fwrite(src_buf + old_pos, 1, res - old_pos, dst);

        global_pos += res;
        res = fread(src_buf, 1, CHUNK_SIZE, src);
    }

    fclose(src);
    fclose(dst);

    return 0;
}
