#ifndef GLOB_H_
#define GLOB_H_

#include <stdint.h>
#include <stdbool.h>
#include <string.h>

#if !defined(GLOB_MALLOC) && !defined(GLOB_FREE)
#include <stdlib.h>
#define GLOB_MALLOC malloc
#define GLOB_FREE free
#elif !defined(GLOB_MALLOC) || !defined(GLOB_FREE)
#error "You must define both GLOB_MALLOC and GLOB_FREE"
#endif

// Matched - falsy
// Not matched for any reason - truthy
typedef enum {
    GLOB_OOM_ERROR      = -4,
    GLOB_ENCODING_ERROR = -3,
    GLOB_SYNTAX_ERROR   = -2,
    GLOB_UNMATCHED      = -1,
    GLOB_MATCHED        =  0,
} Glob_Result;

const char *glob_result_display(Glob_Result result);
Glob_Result glob_utf8(const char *pattern, const char *text);
Glob_Result glob_utf32(const uint32_t *pattern, const uint32_t *text);
// TODO: implement glob_utf16
// TODO: support for non-NULL-terminated (a.k.a sized) strings

#endif // GLOB_H_

#ifdef GLOB_IMPLEMENTATION

// HERE STARTS ConvertUTF CODE //////////////////////////////
/*
 * Copyright 2001-2004 Unicode, Inc.
 * 
 * Disclaimer
 * 
 * This source code is provided as is by Unicode, Inc. No claims are
 * made as to fitness for any particular purpose. No warranties of any
 * kind are expressed or implied. The recipient agrees to determine
 * applicability of information provided. If this file has been
 * purchased on magnetic or optical media from Unicode, Inc., the
 * sole remedy for any claim will be exchange of defective media
 * within 90 days of receipt.
 * 
 * Limitations on Rights to Redistribute This Code
 * 
 * Unicode, Inc. hereby grants the right to freely use the information
 * supplied in this file in the creation of products supporting the
 * Unicode Standard, and to make copies of this file in any form
 * for internal or external distribution as long as this notice
 * remains attached.
 */

/* ---------------------------------------------------------------------
    The following 4 definitions are compiler-specific.
    The C standard does not guarantee that wchar_t has at least
    16 bits, so wchar_t is no less portable than unsigned short!
    All should be unsigned values to avoid sign extension during
    bit mask & shift operations.
------------------------------------------------------------------------ */

typedef unsigned int	UTF32;	/* at least 32 bits */
typedef unsigned short	UTF16;	/* at least 16 bits */
typedef unsigned char	UTF8;	/* typically 8 bits */
typedef unsigned char	Boolean; /* 0 or 1 */

/* Some fundamental constants */
#define UNI_REPLACEMENT_CHAR (UTF32)0x0000FFFD
#define UNI_MAX_BMP (UTF32)0x0000FFFF
#define UNI_MAX_UTF16 (UTF32)0x0010FFFF
#define UNI_MAX_UTF32 (UTF32)0x7FFFFFFF
#define UNI_MAX_LEGAL_UTF32 (UTF32)0x0010FFFF

#define UNI_SUR_HIGH_START  (UTF32)0xD800
#define UNI_SUR_HIGH_END    (UTF32)0xDBFF
#define UNI_SUR_LOW_START   (UTF32)0xDC00
#define UNI_SUR_LOW_END     (UTF32)0xDFFF

typedef enum {
	conversionOK, 		/* conversion successful */
	sourceExhausted,	/* partial character in source, but hit end */
	targetExhausted,	/* insuff. room in target for conversion */
	sourceIllegal		/* source sequence is illegal/malformed */
} ConversionResult;

typedef enum {
	strictConversion = 0,
	lenientConversion
} ConversionFlags;

/*
 * Index into the table below with the first byte of a UTF-8 sequence to
 * get the number of trailing bytes that are supposed to follow it.
 * Note that *legal* UTF-8 values can't have 4 or 5-bytes. The table is
 * left as-is for anyone who may want to do such conversion, which was
 * allowed in earlier algorithms.
 */
static const char trailingBytesForUTF8[256] = {
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
    1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
    2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, 3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5
};

/*
 * Magic values subtracted from a buffer value during UTF8 conversion.
 * This table contains as many values as there might be trailing bytes
 * in a UTF-8 sequence.
 */
static const UTF32 offsetsFromUTF8[6] = { 0x00000000UL, 0x00003080UL, 0x000E2080UL, 
		     0x03C82080UL, 0xFA082080UL, 0x82082080UL };

/*
 * Utility routine to tell whether a sequence of bytes is legal UTF-8.
 * This must be called with the length pre-determined by the first byte.
 * If not calling this from ConvertUTF8to*, then the length can be set by:
 *  length = trailingBytesForUTF8[*source]+1;
 * and the sequence is illegal right away if there aren't that many bytes
 * available.
 * If presented with a length > 4, this returns false.  The Unicode
 * definition of UTF-8 goes up to 4-byte sequences.
 */

static Boolean isLegalUTF8(const UTF8 *source, int length) {
    UTF8 a;
    const UTF8 *srcptr = source+length;
    switch (length) {
    default: return false;
	/* Everything else falls through when "true"... */
    case 4: if ((a = (*--srcptr)) < 0x80 || a > 0xBF) return false;
    case 3: if ((a = (*--srcptr)) < 0x80 || a > 0xBF) return false;
    case 2: if ((a = (*--srcptr)) > 0xBF) return false;

	switch (*source) {
	    /* no fall-through in this inner switch */
	    case 0xE0: if (a < 0xA0) return false; break;
	    case 0xED: if (a > 0x9F) return false; break;
	    case 0xF0: if (a < 0x90) return false; break;
	    case 0xF4: if (a > 0x8F) return false; break;
	    default:   if (a < 0x80) return false;
	}

    case 1: if (*source >= 0x80 && *source < 0xC2) return false;
    }
    if (*source > 0xF4) return false;
    return true;
}

ConversionResult ConvertUTF8toUTF32 (
	const UTF8** sourceStart, const UTF8* sourceEnd, 
	UTF32** targetStart, UTF32* targetEnd, ConversionFlags flags) {
    ConversionResult result = conversionOK;
    const UTF8* source = *sourceStart;
    UTF32* target = *targetStart;
    while (source < sourceEnd) {
	UTF32 ch = 0;
	unsigned short extraBytesToRead = trailingBytesForUTF8[*source];
	if (source + extraBytesToRead >= sourceEnd) {
	    result = sourceExhausted; break;
	}
	/* Do this check whether lenient or strict */
	if (! isLegalUTF8(source, extraBytesToRead+1)) {
	    result = sourceIllegal;
	    break;
	}
	/*
	 * The cases all fall through. See "Note A" below.
	 */
	switch (extraBytesToRead) {
	    case 5: ch += *source++; ch <<= 6;
	    case 4: ch += *source++; ch <<= 6;
	    case 3: ch += *source++; ch <<= 6;
	    case 2: ch += *source++; ch <<= 6;
	    case 1: ch += *source++; ch <<= 6;
	    case 0: ch += *source++;
	}
	ch -= offsetsFromUTF8[extraBytesToRead];

	if (target >= targetEnd) {
	    source -= (extraBytesToRead+1); /* Back up the source pointer! */
	    result = targetExhausted; break;
	}
	if (ch <= UNI_MAX_LEGAL_UTF32) {
	    /*
	     * UTF-16 surrogate values are illegal in UTF-32, and anything
	     * over Plane 17 (> 0x10FFFF) is illegal.
	     */
	    if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_LOW_END) {
		if (flags == strictConversion) {
		    source -= (extraBytesToRead+1); /* return to the illegal value itself */
		    result = sourceIllegal;
		    break;
		} else {
		    *target++ = UNI_REPLACEMENT_CHAR;
		}
	    } else {
		*target++ = ch;
	    }
	} else { /* i.e., ch > UNI_MAX_LEGAL_UTF32 */
	    result = sourceIllegal;
	    *target++ = UNI_REPLACEMENT_CHAR;
	}
    }
    *sourceStart = source;
    *targetStart = target;
    return result;
}

// HERE ENDS ConvertUTF CODE //////////////////////////////

static Glob_Result decode_utf8_with_malloc(const char *in, uint32_t **out)
{
    size_t n = strlen(in);
    *out = GLOB_MALLOC(sizeof(uint32_t)*(n + 1));
    if (*out == NULL) return GLOB_OOM_ERROR;
    memset(*out, 0, sizeof(uint32_t)*(n + 1));
    uint32_t *out_end = *out;

    ConversionResult result = ConvertUTF8toUTF32(
                                  (const UTF8**) &in, (const UTF8*) (in + n),
                                  (UTF32**) &out_end, (UTF32*) out_end + n, 0);
    if (result != conversionOK) return GLOB_ENCODING_ERROR;
    return 0;
}

const char *glob_result_display(Glob_Result result)
{
    switch (result) {
    case GLOB_UNMATCHED:      return "GLOB_UNMATCHED";
    case GLOB_MATCHED:        return "GLOB_MATCHED";
    case GLOB_SYNTAX_ERROR:   return "GLOB_SYNTAX_ERROR";
    case GLOB_ENCODING_ERROR: return "GLOB_ENCODING_ERROR";
    case GLOB_OOM_ERROR:      return "GLOB_OOM_ERROR";
    }
    return NULL;
}

Glob_Result glob_utf8(const char *pattern, const char *text)
{
    Glob_Result result = 0;
    uint32_t *pattern_utf32 = NULL;
    uint32_t *text_utf32 = NULL;

    result = decode_utf8_with_malloc(pattern, &pattern_utf32);
    if (result) goto defer;
    result = decode_utf8_with_malloc(text, &text_utf32);
    if (result) goto defer;
    result = glob_utf32(pattern_utf32, text_utf32);

defer:
    GLOB_FREE(pattern_utf32);
    GLOB_FREE(text_utf32);
    return result;
}

Glob_Result glob_utf32(const uint32_t *pattern, const uint32_t *text)
{
    while (*pattern != '\0' && *text != '\0') {
        switch (*pattern) {
        case '?': {
            pattern += 1;
            text    += 1;
        }
        break;

        case '*': {
            Glob_Result result = glob_utf32(pattern + 1, text);
            if (result != GLOB_UNMATCHED) return result;
            text += 1;
        }
        break;

        case '[': {
            bool matched = false;
            bool negate = false;

            pattern += 1; // skipping [
            if (*pattern == '\0') return GLOB_SYNTAX_ERROR; // unclosed [

            if (*pattern == '!') {
                negate = true;
                pattern += 1;
                if (*pattern == '\0') return GLOB_SYNTAX_ERROR; // unclosed [
            }

            uint32_t prev = *pattern;
            matched |= prev == *text;
            pattern += 1;

            while (*pattern != ']' && *pattern != '\0') {
                switch (*pattern) {
                case '-': {
                    pattern += 1;
                    switch (*pattern) {
                    case ']':
                        matched |= '-' == *text;
                        break;
                    case '\0':
                        return GLOB_SYNTAX_ERROR; // unclosed [
                    default: {
                        matched |= prev <= *text && *text <= *pattern;
                        prev = *pattern;
                        pattern += 1;
                    }
                    }
                }
                break;
                default: {
                    prev = *pattern;
                    matched |= prev == *text;
                    pattern += 1;
                }
                }
            }

            if (*pattern != ']') return GLOB_SYNTAX_ERROR; // unclosed [
            if (negate) matched = !matched;
            if (!matched) return GLOB_UNMATCHED;

            pattern += 1;
            text += 1;
        }
        break;

        case '\\':
            pattern += 1;
            if (*pattern == '\0') return GLOB_SYNTAX_ERROR; // unfinished escape
        // fallthrough
        default: {
            if (*pattern == *text) {
                pattern += 1;
                text    += 1;
            } else {
                return GLOB_UNMATCHED;
            }
        }
        }
    }

    if (*text == '\0') {
        while (*pattern == '*') pattern += 1;
        if (*pattern == '\0') return GLOB_MATCHED;
    }

    return GLOB_UNMATCHED;
}

#endif // GLOB_IMPLEMENTATION
