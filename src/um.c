#include <assert.h>
#include <memory.h>
#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdlib.h>

#include "stack.h"

typedef uint32_t Platter;

typedef struct
{
    Platter len;
    Platter refcount;
    Platter *data;
} Array;

Array *alloc_array(Platter sz)
{
    Array *arr = malloc(sizeof(Array));
    if (nullptr != arr)
    {
        arr->refcount = 1;
        arr->len = sz;
        if (0 == sz)
        {
            arr->data = nullptr;
        }
        else
        {
            arr->data = malloc(sz * sizeof(Platter));
            if (nullptr != arr->data)
            {
                memset(arr->data, 0, sz * sizeof(Platter));
            }
        }
    }

    return arr;
}

Array *dup_array(Array *arr)
{
    if (nullptr == arr)
    {
        return nullptr;
    }

    ++arr->refcount;

    return arr;
}

Array *copy_array(Array *arr)
{
    if (nullptr == arr)
    {
        return nullptr;
    }

    Array *n = alloc_array(arr->len);

    if (nullptr != n)
    {
        memcpy(n->data, arr->data, arr->len * sizeof(Platter));
    }

    return n;
}

void free_array(Array **arr)
{
    if (nullptr == arr)
    {
        return;
    }

    if (nullptr != *arr)
    {
        --((*arr)->refcount);
        if (0 == (*arr)->refcount && 0 != (*arr)->len)
        {
            free((*arr)->data);
        }
    }

    *arr = nullptr;
}

constexpr const Platter cap_increase = 128;

typedef struct
{
    Stack free_indexes;
    Platter max_index;
    Platter capacity;
    Array **arr;
} Memory;

typedef struct
{
    Platter reg[8];
    Platter finger;
    Memory mem;
} Um;

Um *init_machine(Array *prog)
{
    Um *machine = malloc(sizeof(Um));

    if (nullptr != machine)
    {
        machine->finger = 0;

        for (size_t i = 0; i < 8; ++i)
        {
            machine->reg[i] = 0;
        }

        printf("Initialized registers\n");

        machine->mem.free_indexes = malloc(sizeof(Stack));
        assert(nullptr != machine->mem.free_indexes && "Failed to allocate free indexes stack");
        init_stack(machine->mem.free_indexes);

        printf("Initialized stack\n");
        machine->mem.max_index = 1;
        machine->mem.arr = malloc(cap_increase * sizeof(Array *));
        printf("Allocated memory\n");
        if (nullptr != machine->mem.arr)
        {
            machine->mem.capacity = cap_increase;
            for (size_t i = 0; i < cap_increase; ++i)
            {
                machine->mem.arr[i] = nullptr;
            }
            printf("Initialized memory\n");
            machine->mem.arr[0] = prog;
            printf("Set program\n");
        }
    }

    return machine;
}

void free_machine(Um *machine)
{
    assert(nullptr != machine);

    for (size_t i = 0; i < machine->mem.capacity; ++i)
    {
        free_array(&(machine->mem.arr[i]));
        free_stack(machine->mem.free_indexes);
    }

    free(machine);
}

size_t reg_a(Platter op) { return (op >> 6) & 0b111; };
size_t reg_b(Platter op) { return (op >> 3) & 0b111; };
size_t reg_c(Platter op) { return op & 0b111; };

void conditional_move(Um *um, Platter op)
{
    size_t a = reg_a(op);
    size_t b = reg_b(op);
    size_t c = reg_c(op);

    if (0 == um->reg[c])
    {
        um->reg[a] = um->reg[b];
    }

    ++um->finger;
}

void array_index(Um *um, Platter op)
{
    size_t a = reg_a(op);
    size_t b = reg_b(op);
    size_t c = reg_c(op);

    assert(nullptr != um->mem.arr[um->reg[b]] && "Attempt to index nonexistent array");
    assert(um->reg[c] < um->mem.arr[um->reg[b]]->len && "Attempt to index outside array capacity");

    um->reg[a] = um->mem.arr[um->reg[b]]->data[c];

    ++um->finger;
}

void array_amendment(Um *um, Platter op)
{
    size_t a = reg_a(op);
    size_t b = reg_b(op);
    size_t c = reg_c(op);

    assert(nullptr != um->mem.arr[um->reg[a]] && "Attempt to amend nonexistent array");
    assert(um->reg[b] < um->mem.arr[um->reg[a]]->len && "Attempt to amend outside array capacity");

    um->mem.arr[um->reg[a]]->data[b] = um->reg[c];

    ++um->finger;
}

void addition(Um *um, Platter op)
{
    size_t a = reg_a(op);
    size_t b = reg_b(op);
    size_t c = reg_c(op);

    um->reg[a] = um->reg[b] + um->reg[c];

    ++um->finger;
}

void multiplication(Um *um, Platter op)
{
    size_t a = reg_a(op);
    size_t b = reg_b(op);
    size_t c = reg_c(op);

    um->reg[a] = um->reg[b] * um->reg[c];

    ++um->finger;
}

void division(Um *um, Platter op)
{
    size_t a = reg_a(op);
    size_t b = reg_b(op);
    size_t c = reg_c(op);

    um->reg[a] = um->reg[b] / um->reg[c];

    ++um->finger;
}

void not_and(Um *um, Platter op)
{
    size_t a = reg_a(op);
    size_t b = reg_b(op);
    size_t c = reg_c(op);

    um->reg[a] = ~(um->reg[b] & um->reg[c]);

    ++um->finger;
}

void allocation(Um *um, Platter op)
{
    size_t b = reg_b(op);
    size_t c = reg_c(op);

    Platter idx;
    if (is_empty(um->mem.free_indexes))
    {
        idx = pop(um->mem.free_indexes);
    }
    else
    {
        idx = um->mem.max_index++;
    }

    if (idx >= um->mem.capacity)
    {
        size_t sz = um->mem.capacity + cap_increase;
        Array **arr = realloc(um->mem.arr, sz);

        assert(nullptr != arr && "Failure to reallocate arrays");

        um->mem.arr = arr;

        for (size_t i = um->mem.capacity; i < sz; ++i)
        {
            um->mem.arr[i] = nullptr;
        }

        um->mem.capacity = sz;
    }

    um->mem.arr[idx] = alloc_array(um->reg[c]);
    um->reg[b] = idx;

    ++um->finger;
}

void abandonment(Um *um, Platter op)
{
    size_t c = reg_c(op);

    assert(0 != um->reg[c] && "Attempt to abandon 0 array");
    assert(nullptr != um->mem.arr[um->reg[c]] && "Attempt to abandon nonexistent array");

    free_array(um->mem.arr + um->reg[c]);
    push(um->mem.free_indexes, um->reg[c]);

    ++um->finger;
}

void output(Um *um, Platter op)
{
    size_t c = reg_c(op);

    putc(um->reg[c] & 0xFF, stdout);

    ++um->finger;
}

void input(Um *um, Platter op)
{
    size_t c = reg_c(op);

    um->reg[c] = getc(stdin) & 0xFF;

    ++um->finger;
}

void load_program(Um *um, Platter op)
{
    size_t b = reg_b(op);
    size_t c = reg_c(op);

    if (0 != um->reg[b])
    {
        assert(um->reg[b] < um->mem.capacity && "Attempt to load program from nonexistent array");
        assert(nullptr != um->mem.arr[um->reg[b]] && "Attempt to load program from nonexistent array");

        Array *arr = copy_array(um->mem.arr[um->reg[b]]);

        free_array(um->mem.arr);
        um->mem.arr[0] = arr;
    }

    um->finger = um->reg[c];
}

void orthography(Um *um, Platter op)
{
    size_t a = (op >> 25) & 0b111;
    size_t val = op & 0x01FFFFFF;

    um->reg[a] = val;

    ++um->finger;
}

int main(int argc, char **argv)
{
    long sz = 0;
    for (int i = 1; i < argc; ++i)
    {
        FILE *f = fopen(argv[i], "rb");
        if (nullptr != f)
        {
            fseek(f, 0, SEEK_END);
            sz += ftell(f);
            fclose(f);
        }
    }

    Array *prog = alloc_array(sz / sizeof(Platter));
    Platter j = 0;

    for (int i = 1; i < argc; ++i)
    {
        FILE *f = fopen(argv[i], "rb");

        if (nullptr == f)
        {
            continue;
        }

        constexpr const size_t SIZE = 4;
        uint8_t buf[SIZE] = {};
        size_t result = 0;

        while (SIZE == (result = fread(buf, sizeof(buf[0]), SIZE, f)))
        {
            prog->data[j++] = buf[0] << 24 | buf[1] << 16 | buf[2] << 8 | buf[3];
        }

        fclose(f);
    }

    printf("Program loaded, len: %" PRIu32 ", refcount: %" PRIu32 "\n", prog->len, prog->refcount);

    Um *um = init_machine(prog);

    printf("Machine initalized\n");

    while (true)
    {
        assert(um->finger < um->mem.arr[0]->len && "Execution finget points outside program");

        Platter op = um->mem.arr[0]->data[um->finger];
        switch (op >> 28)
        {
        case 0:
            conditional_move(um, op);
            break;
        case 1:
            array_index(um, op);
            break;
        case 2:
            array_amendment(um, op);
            break;
        case 3:
            addition(um, op);
            break;
        case 4:
            multiplication(um, op);
            break;
        case 5:
            division(um, op);
            break;
        case 6:
            not_and(um, op);
            break;
        case 7:
            printf("Machine halted");
            goto end_of_program;
            break;
        case 8:
            allocation(um, op);
            break;
        case 9:
            abandonment(um, op);
            break;
        case 10:
            output(um, op);
            break;
        case 11:
            input(um, op);
            break;
        case 12:
            load_program(um, op);
            break;
        case 13:
            orthography(um, op);
            break;
        default:
            assert(false && "Unknown operation");
        }
    }

end_of_program:

    free_machine(um);

    return 0;
}
