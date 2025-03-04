#pragma once

#include <prelude/types.h>

_BEGIN_DECLS

#define _esl_opcmpeq_u32_u32(a, b) (a == b)
#define _esl_opcmpneq_u32_u32(a, b) (a != b)
#define _esl_opcmplt_u32_u32(a, b) (a < b)
#define _esl_opcmplte_u32_u32(a, b) (a <= b)
#define _esl_opcmpgt_u32_u32(a, b) (a > b)
#define _esl_opcmpgte_u32_u32(a, b) (a >= b)

#define _esl_opcmpeq_char_char(a, b) (a.cp == b.cp)
#define _esl_opcmpneq_char_char(a, b) (a.cp != b.cp)
#define _esl_opcmplt_char_char(a, b) (a.cp < b.cp)
#define _esl_opcmplte_char_char(a, b) (a.cp <= b.cp)
#define _esl_opcmpgt_char_char(a, b) (a.cp > b.cp)
#define _esl_opcmpgte_char_char(a, b) (a.cp >= b.cp)

esl_u32 _esl_opadd_u32_u32(esl_u32 a, esl_u32 b);
esl_u32 _esl_opsub_u32_u32(esl_u32 a, esl_u32 b);
esl_u32 _esl_opmul_u32_u32(esl_u32 a, esl_u32 b);
esl_u32 _esl_opdiv_u32_u32(esl_u32 a, esl_u32 b);
esl_u32 _esl_opmod_u32_u32(esl_u32 a, esl_u32 b);

#define _esl_opass_u32_u32(aptr, b) (void)(*aptr = b)
#define _esl_opassadd_u32_u32(aptr, b) (void)(*aptr = _esl_opadd_u32_u32(*aptr, b))
#define _esl_opasssub_u32_u32(aptr, b) (void)(*aptr = _esl_opsub_u32_u32(*aptr, b))
#define _esl_opassmul_u32_u32(aptr, b) (void)(*aptr = _esl_opmul_u32_u32(*aptr, b))
#define _esl_opassdiv_u32_u32(aptr, b) (void)(*aptr = _esl_opdiv_u32_u32(*aptr, b))
#define _esl_opassmod_u32_u32(aptr, b) (void)(*aptr = _esl_opmod_u32_u32(*aptr, b))

#define _esl_opass_char_char(aptr, b) (void)(*aptr = b)

#define _esl_opand_bool_bool(a, b) (a && b)
#define _esl_opor_bool_bool(a, b) (a || b)

#define _esl_oprange_u32_u32(a, b) ((esl_range) { (esl_usize)a, (esl_usize)b })

_END_DECLS
