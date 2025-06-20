#include <assert.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

#include "stack.h"

void print_stack(Stack st)
{
    assert(nullptr != st && "Need valid pointer to stack head");

    printf("Stack head. data: %" PRIu32 ", next: %p\n", st->data, st->next);
    if (is_empty(st))
    {
        printf("  Stack is empty\n");
    }
    else
    {
        printf("  Stack structure: ");
        StNode *node = st->next;
        while (nullptr != node)
        {
            printf("(%" PRIu32 ", %p),", node->data, node->next);
            node = node->next;
        }
        printf("\n");
    }
}

int main(int argc, char **argv)
{
    Stack st = malloc(sizeof(Stack));
    assert(nullptr != st && "Failed to allocate stack");
    init_stack(st);
    printf("Stack initialised. data: %" PRIu32 ", next: %p\n\n", st->data, st->next);

    printf("Pushing data into stack:\n");
    print_stack(st);
    push(st, 1);
    print_stack(st);
    push(st, 2);
    print_stack(st);

    uint32_t data = pop(st);
    printf("Got data from stack: %" PRIu32 "\n", data);
    print_stack(st);
    data = pop(st);
    printf("Got data from stack: %" PRIu32 "\n", data);
    print_stack(st);

    push(st, 2);
    print_stack(st);
    free_stack(st);
}
