#include <esl_header.h>

#include <cstdlib>

extern "C" {

void _esl_panic(const char* msg) {
    fprintf(stderr, "Panic: %s\n", msg);
    abort();
}

esl_format_args* _esl_format_args_push(esl_format_args* end, esl_format_args_data data) {
    esl_format_args* new_node = (esl_format_args*)malloc(sizeof(esl_format_args));
    new_node->node = data;
    new_node->next = nullptr;
    if (end) {
        end->next = new_node;
    }
    return new_node;
}

static void args_free(esl_format_args* list) {
    while (list) {
        esl_format_args* next = list->next;
        free(list);
        list = next;
    }
}

void _esl_print(const char* fmtstr, esl_format_args* args) {
    // replace every "{}" in fmtstr with the next argument
    while (*fmtstr) {
        if (*fmtstr == '{' && fmtstr[1] == '}') {
            if (!args) {
                _esl_panic("Not enough arguments passed for format string");
            }
            args->node.print(args->node.data);
            args = args->next;
            fmtstr += 2;
        }
        else {
            putchar(*fmtstr);
            fmtstr++;
        }
    }
    args_free(args);
}

void _esl_print_u32(void* data) {
    printf("%u", *(esl_u32*)data);
}

void _esl_print_static_string(void* data) {
    printf("%s", (const char*)data);
}
}
