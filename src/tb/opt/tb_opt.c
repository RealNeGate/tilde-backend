#include "../tb_internal.h"

#if TB_HOST_ARCH == TB_HOST_X86_64
// Needed for some of the fancier 
#include <x86intrin.h>
#endif

#include "tb_opt_canonical.h"
#include "tb_opt_dce.h"
#include "tb_opt_inline.h"
#include "tb_opt_fold.h"
#include "tb_opt_mem2reg.h"
#include "tb_opt_hoist_locals.h"
#include "tb_opt_sr.h"
#include "tb_opt_deshort_circuit.h"
#include "tb_opt_copy_elision.h"
