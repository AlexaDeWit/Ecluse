// SPDX-FileCopyrightText: 2026 Alexandra de Wit
//
// SPDX-License-Identifier: MIT

/*
 * libFuzzer harness: runs the vendored lexer (vendor/json-stream/c_lib/lexer.c) and upstream's
 * lexer at 537a43a7 (lexer-537a43a7.c) side by side, and aborts on any difference in return code,
 * lexer header, or result buffer. Pieces are fed the way Data.JsonStream.CLexer feeds them.
 * Build and run with scripts/fuzz-json-lexer.sh.
 *
 * Input layout: byte 0 selects the piece count (low 3 bits) and a small result limit (bit 3),
 * byte 1 is the small limit, then two bytes per split offset, then the JSON bytes.
 * Every input also runs as one piece and as one byte per piece.
 *
 * The vendored lexer reads each piece from an allocation of exactly the piece's length, so
 * AddressSanitizer reports any read past it. Upstream's lexer gets one spare byte, because its
 * string loop reads the byte after a piece that ends inside a string (define UPSTREAM_EXACT to
 * see that report).
 */
#include <locale.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lexer.h"

int upstream_lex_json(const char *input, struct lexer *lexer, struct lexer_result *result);

#define POISON 0xA5

static void fail(const char *what, size_t piece, int call)
{
  fprintf(stderr, "lexer difference: %s (piece %zu, call %d)\n", what, piece, call);
  abort();
}

/* Mirrors estResultLimit in vendor/json-stream/Data/JsonStream/CLexer.hs. */
static int est_result_limit(size_t len)
{
  return 20 + (int) (len / 5);
}

/* Returns 0 to continue with the next piece, 1 when both lexers stopped with an error. */
static int run_piece(const uint8_t *bytes, size_t len, struct lexer *hdr, int small_limit,
                     size_t piece)
{
  hdr->position = 0;
  hdr->length = (int) len;
  hdr->result_limit = small_limit ? small_limit : est_result_limit(len);

  char *patched_in = malloc(len ? len : 1);
  char *stock_in = malloc(len + 1);
  memcpy(patched_in, bytes, len);
  memcpy(stock_in, bytes, len);
  stock_in[len] = 0;
#ifdef UPSTREAM_EXACT
  free(stock_in);
  stock_in = malloc(len ? len : 1);
  memcpy(stock_in, bytes, len);
#endif

  size_t res_bytes = (size_t) hdr->result_limit * sizeof(struct lexer_result);
  struct lexer_result *patched_res = malloc(res_bytes);
  struct lexer_result *stock_res = malloc(res_bytes);
  int status = 0;

  for (int call = 0; hdr->position < hdr->length; call++) {
    struct lexer patched_hdr = *hdr, stock_hdr = *hdr;
    patched_hdr.result_num = stock_hdr.result_num = 0;
    memset(patched_res, POISON, res_bytes);
    memset(stock_res, POISON, res_bytes);

    int stock_rc = upstream_lex_json(stock_in, &stock_hdr, stock_res);
    int patched_rc = lex_json(patched_in, &patched_hdr, patched_res);

    if (stock_rc != patched_rc)
      fail("return code", piece, call);
    if (memcmp(&stock_hdr, &patched_hdr, sizeof stock_hdr) != 0)
      fail("header", piece, call);
    if (memcmp(stock_res, patched_res, res_bytes) != 0)
      fail("results", piece, call);

    int progressed = stock_hdr.position != hdr->position || stock_hdr.result_num != 0;
    *hdr = stock_hdr;
    if (stock_rc != LEX_OK) {
      status = 1;
      break;
    }
    if (!progressed)
      break; /* Both lexers stall the same way: parity holds, stop here. */
  }

  free(patched_in);
  free(stock_in);
  free(patched_res);
  free(stock_res);
  return status;
}

static void run_pieces(const uint8_t *bytes, size_t len, const size_t *cuts, size_t ncuts,
                       int small_limit)
{
  struct lexer hdr;
  memset(&hdr, 0, sizeof hdr);
  size_t start = 0;
  for (size_t i = 0; i <= ncuts; i++) {
    size_t end = i < ncuts ? cuts[i] : len;
    if (run_piece(bytes + start, end - start, &hdr, small_limit, i))
      return;
    start = end;
  }
}

static int compare_size(const void *a, const void *b)
{
  size_t x = *(const size_t *) a, y = *(const size_t *) b;
  return (x > y) - (x < y);
}

int LLVMFuzzerInitialize(int *argc, char ***argv)
{
  (void) argc;
  (void) argv;
  /* The GHC runtime takes LC_CTYPE from the environment, so the harness does too. */
  setlocale(LC_CTYPE, "");
  return 0;
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
  if (size < 2)
    return 0;
  size_t ncuts = data[0] & 7;
  int small_limit = (data[0] & 8) ? 1 + (data[1] & 3) : 0;
  size_t header = 2 + 2 * ncuts;
  if (size < header)
    return 0;
  const uint8_t *bytes = data + header;
  size_t len = size - header;

  size_t cuts[8];
  for (size_t i = 0; i < ncuts; i++)
    cuts[i] = ((size_t) data[2 + 2 * i] << 8 | data[3 + 2 * i]) % (len + 1);
  qsort(cuts, ncuts, sizeof cuts[0], compare_size);

  run_pieces(bytes, len, cuts, ncuts, small_limit);
  run_pieces(bytes, len, NULL, 0, small_limit);

  size_t *singles = malloc((len ? len : 1) * sizeof *singles);
  for (size_t i = 0; i + 1 < len; i++)
    singles[i] = i + 1;
  run_pieces(bytes, len, singles, len ? len - 1 : 0, small_limit);
  free(singles);
  return 0;
}
