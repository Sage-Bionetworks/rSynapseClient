/* Initialize c functions for curl reader/writer in R
 *
 * Author: Martin Morgan <mtmorgan@fhcrc.org>
 */
#ifndef WRITER_H
#define WRITER_H

SEXP writer_open(SEXP filename);
SEXP writer_close(SEXP ext);

SEXP reader_open(SEXP filename);
SEXP reader_close(SEXP ext);

#endif
