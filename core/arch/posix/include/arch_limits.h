#pragma once

#define __PAGE_SHIFT	12
#define __PAGE_SIZE	(1 << __PAGE_SHIFT)

#define STACK_SIZE_ORDER	10
#define STACK_SIZE	((__PAGE_SIZE) << STACK_SIZE_ORDER)

