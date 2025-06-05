/*---------------------------------------------------------------------------
   Copyright (c) 2013 The tgls programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

/* Compile with:
   gcc -o assert_sizes assert_sizes.c

   The following assertions should hold to be sure that the bindings
   work correctly. */

#include <assert.h>
#include <stddef.h>

int main (void)
{

  assert (sizeof (int) == 4);
  assert (sizeof (unsigned int) == 4);

  if (sizeof (void *) == 4)
    {
      assert (sizeof (ptrdiff_t) == 4);
    }
  else if (sizeof (void *) == 8)
    {
      assert (sizeof (ptrdiff_t) == 8);
    }
  else
    {
      assert (0);
    }
  return 0;
}
