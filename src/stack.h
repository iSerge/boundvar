#pragma once

#include <stdint.h>

typedef struct StNode_t
{
    uint32_t data;
    struct StNode_t *next;
} StNode;

typedef struct StNode_t *Stack;

uint32_t pop(Stack stack);
void push(Stack stack, uint32_t data);
bool is_empty(Stack stack);
void init_stack(Stack stack);
void free_stack(Stack stack);
