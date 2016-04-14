/* curl reader/writer function for use by RCurl package
 *
 * Author: Martin Morgan <mtmorgan@fhcrc.org>
 */

#include <Rdefines.h>
#include <stdio.h>
#include <stdlib.h>

/* writer_open and writer_close manage the 'external pointer' that
 * references the C-level pointer to the opened file. _writer_finalize
 * is called explicitly or implicitly (when the external pointer is
 * garbage collected) to close the file.
 */

static void
_writer_finalizer(SEXP ext)
{
    if (NULL == R_ExternalPtrAddr(ext))
        return;
    FILE *file = (FILE *) R_ExternalPtrAddr(ext);
    int res = fclose(file);
    if (0 != res) {
        /* FIXME: errno */
        Rf_error("'writer' internal: failed to close");
    }
    R_SetExternalPtrAddr(ext, NULL);
}

static void
_reader_finalizer(SEXP ext)
{
    if (NULL == R_ExternalPtrAddr(ext))
        return;
    FILE *file = (FILE *) R_ExternalPtrAddr(ext);
    int res = fclose(file);
    if (0 != res) {
        /* FIXME: errno */
        Rf_error("'reader' internal: failed to close");
    }
    R_SetExternalPtrAddr(ext, NULL);
}

SEXP
writer_open(SEXP filename)
{
    FILE *file;
    SEXP ext;

    if (!isString(filename) || 1 != Rf_length(filename))
        Rf_error("'filename' must be character(1)");

    file = fopen(translateChar(STRING_ELT(filename, 0)), "ab");
    if (NULL == file) {
        /* FIXME: errno */
        Rf_error("'writer' failed to open file '%s'",
                 translateChar(STRING_ELT(filename, 0)));
    }

    ext = PROTECT(R_MakeExternalPtr(file, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ext, _writer_finalizer, TRUE);
    UNPROTECT(1);
    return ext;
}

SEXP
reader_open(SEXP filename)
{
	FILE *file;
	SEXP ext;
	
	if (!isString(filename) || 1 != Rf_length(filename))
        Rf_error("'filename' must be character(1)");
	
    file = fopen(translateChar(STRING_ELT(filename, 0)), "rb");
    if (NULL == file) {
        /* FIXME: errno */
        Rf_error("'writer' failed to open file '%s'",
                 translateChar(STRING_ELT(filename, 0)));
    }
	
	ext = PROTECT(R_MakeExternalPtr(file, R_NilValue, R_NilValue));
    R_RegisterCFinalizerEx(ext, _writer_finalizer, TRUE);
    UNPROTECT(1);
    return ext;
}

SEXP
writer_close(SEXP ext)
{
    _writer_finalizer(ext);
    return R_NilValue;
}

SEXP
reader_close(SEXP ext)
{
    _reader_finalizer(ext);
    return R_NilValue;
}

/* _writer_write is called by curl to write the data
 */

size_t
_writer_write(void *buffer, size_t size, size_t nmemb, void *data)
{
    FILE *file = (FILE *) data;
    size_t len;

    if (NULL == file) {
        /* Rf_warn signals something wrong but allows the curl library
         * to recover.
         *
         * FIXME: wonder what happens when curl library errors?
         */
        Rf_warning("'writer' internal: NULL FILE pointer");
        return 0;              /* trigger error in curl? */
    }

    len  = fwrite(buffer, size, nmemb, file);
    if (len != nmemb)
        /* Rf_warning here; error in curl library.
         *
         * FIXME: wonder what happens when curl library errors?
         */
        Rf_warning("'writer' internal: bytes written != bytes in buffer");

    return len;
}


size_t
_reader_read(void *buffer, size_t size, size_t nmemb, void *data)
{
	FILE *file = (FILE *) data;
	size_t len;
	
	if (NULL == file) {
        /* Rf_warn signals something wrong but allows the curl library
         * to recover.
         *
         * FIXME: wonder what happens when curl library errors?
         */
        Rf_warning("'reader' internal: NULL FILE pointer");
        return 0;              /* trigger error in curl? */
    }
	
	len = fread(buffer, size, nmemb, file);
	/*
	 * FIXME: is there a way to check that the read worked?
	 */
	
    return len;
}

/*
*------------------------------------------------------------
*/

struct chunk_data {
	Rbyte *data;
	size_t len;
	size_t next_elem;
};

static void
_string_reader_finalizer(SEXP ext)
{
    if (NULL == R_ExternalPtrAddr(ext))
        return;
    struct chunk_data *data = (struct chunk_data *) R_ExternalPtrAddr(ext);
    printf("\nIn _string_reader_finalizer. About to free allocated 'chunk_data'.\n");
    free(data->data);
    free(data);
    R_SetExternalPtrAddr(ext, NULL);
}


/*
* read from a string buffer
* data is a list with three fields (1) byte buffer, (2) buffer length, (3) current pos'n
*/
size_t
_string_reader_read(void *buffer, size_t size, size_t nmemb, void *data)
{
	struct chunk_data *chunk  = (struct chunk_data *) data;
	const size_t chunk_size = (*chunk).len;
	size_t next_byte = (*chunk).next_elem;
	const size_t len = chunk_size-next_byte < size*nmemb ? chunk_size-next_byte : size*nmemb;
	
	if (next_byte==0 || next_byte>=chunk_size) {
		printf("\nIn _string_reader_read: size=%ld, nmemb=%ld, chunk_size=%ld, next_byte=%ld, len=%ld, data ptr: %ld\n",
			size, nmemb, chunk_size, next_byte, len, data);
	}
		
	memcpy(buffer, ((*chunk).data+next_byte), len);
	
	(*chunk).next_elem += len; 
	
    return len;
}


SEXP
create_string_data(SEXP data)
{
	struct chunk_data *cd = (struct chunk_data*)malloc(sizeof(struct chunk_data));
	SEXP ext;

	cd->data = (Rbyte *)malloc(sizeof(Rbyte) * LENGTH(data));
	cd->len = LENGTH(data);
	cd->next_elem = 0;
	
	printf("\nIn create_string_data: created a buffer of length: %d\n", LENGTH(data));
	
	if (!IS_RAW(data)) Rf_error("'data' must be raw");
	
	memcpy(cd->data, RAW(data), LENGTH(data));
	
	ext = PROTECT(R_MakeExternalPtr(cd, R_NilValue, R_NilValue));
	// add a finalizer which cleans up the object
    R_RegisterCFinalizerEx(ext, _string_reader_finalizer, TRUE);
    UNPROTECT(1);
    return ext;
}
