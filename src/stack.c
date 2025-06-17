#include <assert.h>
#include <stdlib.h>
#include "stack.h"

uint32_t pop(Stack stack)
{
    assert(nullptr != stack);
    if (is_empty(stack))
    {
        assert(nullptr != stack->next);
        return -1;
    }
    else
    {
        StNode *node = stack->next;
        uint32_t data = node->data;
        stack->next = node->next;
        free(node);
        return data;
    }
}

void push(Stack stack, uint32_t data)
{
    assert(nullptr != stack);
    if (is_empty(stack))
    {
        assert(nullptr != stack->next);
    }
    else
    {
        StNode *node = malloc(sizeof(StNode));
        assert(nullptr != node);

        node->data = data;
        node->next = stack->next;
        stack->next = node;
    }
}

bool is_empty(Stack stack) { return nullptr == stack || nullptr == stack->next; }

void init_stack(Stack stack)
{
    assert(nullptr != stack);
    stack->data = 0;
    stack->next = nullptr;
}

void free_stack(Stack stack)
{
    assert(nullptr != stack);

    while (!is_empty(stack))
    {
        pop(stack);
    }

    free(stack);
}
