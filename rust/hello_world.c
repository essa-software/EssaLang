#include <esl_header.h>

/* function main (ID=1_1, params scope ID=1_1)*/
esl_u32 $$esl_main();

/* function print (ID=0_0, params scope ID=1_0)*/
void print(esl_static_string fmtstr);

esl_u32 $$esl_main() {
{
    /* var decl str */
    esl_static_string str0;
    
esl_static_string $$tmp1_lit;
$$tmp1_lit = "Hello, world!\n";
    str0 = $$tmp1_lit;
print(str0);
}
}
int main() {
    return $$esl_main();
}
