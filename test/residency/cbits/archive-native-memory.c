/* SPDX-FileCopyrightText: 2026 Alexandra de Wit
 * SPDX-License-Identifier: MIT
 */

#include <stddef.h>
#include <stdlib.h>

#if defined(__linux__) && defined(__GLIBC__)
#if __GLIBC_PREREQ(2, 33)
#include <malloc.h>
#define ECLUSE_HAS_MALLINFO2 1
#endif
#endif

/* Keep the libc struct layout inside C. Unsupported platforms cannot report a passing zero. */
int ecluse_archive_allocator_snapshot(size_t *arena_bytes, size_t *mapped_bytes)
{
#if defined(ECLUSE_HAS_MALLINFO2)
    const struct mallinfo2 snapshot = mallinfo2();
    *arena_bytes = snapshot.uordblks;
    *mapped_bytes = snapshot.hblkhd;
    return 1;
#else
    (void)arena_bytes;
    (void)mapped_bytes;
    return 0;
#endif
}
