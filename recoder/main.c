#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define CHUNK_SIZE 4096
#define NELEMS(x)  (sizeof(x) / sizeof((x)[0]))
#define LOG(...)  fprintf(stderr, __VA_ARGS__)

const char STORE_OPCODES[] = {
    0x8d,   // sta $ffff
    0x9d,   // sta $ffff, x
    0x99,   // sta $ffff, y
    0x8e,   // stx $ffff
    0x8c    // sty $ffff
};
const char JSR_OPCODE = 0x20;

// TODO: These should be a CLI options
const unsigned int JSR_ADDR = 0xc500;
const char* BASE_LABEL = "SID_regs_base";


static int search_SID_stores(const char* buf, unsigned int size, unsigned int stores_pos[]);
static void write_store(const char* code, const unsigned int oper, FILE* f);
static void write_sta(const unsigned int oper, FILE* f);
static void write_sta_x(const unsigned int oper, FILE* f);
static void write_sta_y(const unsigned int oper, FILE* f);
static void write_stx(const unsigned int oper, FILE* f);
static void write_sty(const unsigned int oper, FILE* f);
static int compare(const void *a, const void *b);


int main(int argc, char *argv[]) {
    if (argc != 4) {
        LOG("Usage: %s SID FIXED_SID GEN_ASM\n", argv[0]);
        return 1;
    }

    const char* src_path = argv[1];
    const char* dst_path = argv[2];
    const char* gen_asm_path = argv[3];

    FILE* src = fopen(src_path, "rb");
    if (!src) {
        LOG("Failed to open %s\n for reading", src_path);
        return 2;
    }

    // TODO: Make sure its a valid SID file
    // ...

    // Get file size
    fseek(src, 0, SEEK_END);
    unsigned int size = ftell(src);
    rewind(src);

    LOG("File size: %u\n", size);

    // TODO: Get offsetData from header
    // fseek(src, 0x7c, SEEK_SET);
    // ...

    // Allocate buffer for .sid file
    char* src_buf = malloc(size * sizeof(char));
    if (!src_buf) {
        LOG("Failed to allocate memory for src_buf\n");
        fclose(src);
        return 3;
    }

    // Read file contents into buffer and close file
    size_t res = fread(src_buf, 1, size, src);
    if (!res) {
        LOG("Failed to read file\n");
        free(src_buf);
        fclose(src);
        return 3;
    }
    fclose(src);

    // Allocate for stores_pos array
    unsigned int* stores_pos = malloc(size * sizeof(unsigned int));
    if (!stores_pos) {
        LOG("Failed to read file\n");
        free(src_buf);
        return 3;
    }

    // Look for stores to SID registers and track them
    int stores = search_SID_stores(src_buf, size, stores_pos);
    LOG("SID Stores: %d\n", stores);

    free(src_buf);
    free(stores_pos);

    /*
    char src_buf[CHUNK_SIZE] = "";

    int stores_pos[CHUNK_SIZE / 3] = {};
    int stores = 0;

    unsigned int global_pos = 0;
    size_t res = fread(src_buf, 1, CHUNK_SIZE, src);
    while (res > 0) {
        src_buf[res] = 0;

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

                    LOG("Found write ($%x) to SID register $%x at $%x\n", opcode, operand, global_pos + pos);
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

            //char* ptr = src_buf + stores_pos[i];
            //int opcode  = ptr[0] & 0xff;
            //int op_lo  = ptr[1] & 0xff;

            unsigned char extra_store[3] = {
                JSR_OPCODE,
                JSR_ADDR & 0xff,
                (JSR_ADDR >> 8) & 0xff,
            };
            fwrite(extra_store, 1, 3, dst);

            old_pos = stores_pos[i] + 3;
        }
        fwrite(src_buf + old_pos, 1, res - old_pos, dst);

        global_pos += res;
        res = fread(src_buf, 1, CHUNK_SIZE, src);
    }

    // Example:
    write_sta(0xd400, gen_asm);
    write_sta_x(0xd405, gen_asm);
    write_sta_y(0xd406, gen_asm);
    write_stx(0xd416, gen_asm);
    write_sty(0xd412, gen_asm);
    */

    /*
    FILE* dst = fopen(dst_path, "wb");
    if (!dst) {
        LOG("Failed to open %s for writing\n", dst_path);
        return 2;
    }

    FILE* gen_asm = fopen(gen_asm_path, "wb");
    if (!gen_asm) {
        LOG("Failed to open %s for writing\n", gen_asm_path);
        return 2;
    }

    fclose(dst);
    fclose(gen_asm);
    */

    return 0;
}


static int search_SID_stores(const char* buf, unsigned int size, unsigned int stores_pos[]) {
    int count = 0;

    // Search for Stores on SID registers and save their locations on
    // stores_pos array.
    for (int i = 0; i < NELEMS(STORE_OPCODES); i++) {
        const char* ptr = memchr(buf, STORE_OPCODES[i], size);

        while (ptr) {
            // These instructions have a 16-bit operand (little-endian)
            // FIXME: Save operand in a single variable
            int op_hi = ptr[2] & 0xff;
            int op_lo = ptr[1] & 0xff;

            if (op_hi == 0xd4 && op_lo >= 0 && op_lo < 0x20) {
                int opcode  = ptr[0] & 0xff;
                int operand = (op_lo | op_hi << 8) & 0xffff;
                int pos = ptr - buf;

                stores_pos[count++] = pos;

                LOG("Found write ($%x) to SID register $%x at $%x\n", opcode, operand, pos);
            }
            ptr = memchr(ptr + 3, STORE_OPCODES[i], size);
        }
    }

    // Sort stores_pos array
    qsort(stores_pos, count, sizeof(int), compare);

    return count;
}

static int compare(const void *a, const void *b) {
  return (*(int*)a - *(int*)b);
}

static void write_store(const char* code, const unsigned int oper, FILE* f) {
    fprintf(f, code, oper & 0xffff, BASE_LABEL, oper & 0xff);
}

static void write_sta(const unsigned int oper, FILE* f) {
    write_store("\tsta $%x\n" \
                "\tsta %s + $%x\n" \
                "\trts\n" \
                "\t.byte 0\n\n", oper, f);
}

static void write_sta_x(const unsigned int oper, FILE* f) {
    write_store("\tsta $%x, x\n" \
                "\tsta %s + $%x, x\n" \
                "\trts\n" \
                "\t.byte 0\n\n", oper, f);
}

static void write_sta_y(const unsigned int oper, FILE* f) {
    write_store("\tsta $%x, y\n" \
                "\tsta %s + $%x, y\n" \
                "\trts\n" \
                "\t.byte 0\n\n", oper, f);
}

static void write_stx(const unsigned int oper, FILE* f) {
    write_store("\tstx $%x\n" \
                "\tstx %s + $%x\n" \
                "\trts\n" \
                "\t.byte 0\n\n", oper, f);
}

static void write_sty(const unsigned int oper, FILE* f) {
    write_store("\tsty $%x\n" \
                "\tsty %s + $%x\n" \
                "\trts\n" \
                "\t.byte 0\n\n", oper, f);
}
