#include <esl_header.h>
void print(esl_static_string fmtstr);
esl_u32 $$esl_main();
esl_u32 $$esl_main() {
{
    
esl_static_string $$tmp1_lit;
$$tmp1_lit = "Hello, world!\n";
print($$tmp1_lit);
}
}
int main() {
    return $$esl_main();
}
