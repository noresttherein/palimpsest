package net.turambar.commune

import java.nio.charset.Charset

/**
  * @author Marcin Mościcki
  */
/*
class CharsText extends Text {
	/** The value is used for character storage. */
	private var value: Array[Char] = null
	/** Cache the hash code for the string */
	private var hash: Int = 0 // Default to 0
	/** use serialVersionUID from JDK 1.0.2 for interoperability */
	private val serialVersionUID: Long = -6849794470754667710L
	/**
	  * Class String is special cased within the Serialization Stream Protocol.
	  *
	  * A String instance is written into an ObjectOutputStream according to
	  * <a href="{@docRoot }/../platform/serialization/spec/output.html">
	  * Object Serialization Specification, Section 6.2, "Stream Elements"</a>
	  */
	private val serialPersistentFields: Array[ObjectStreamField] = new Array[ObjectStreamField](0)
	
	/**
	  * Initializes a newly created {@code String} object so that it represents
	  * an empty character sequence.  Note that use of this constructor is
	  * unnecessary since Strings are immutable.
	  */
	def this {
		this()
		this.value = "".value
	}
	
	/**
	  * Initializes a newly created {@code String} object so that it represents
	  * the same sequence of characters as the argument; in other words, the
	  * newly created string is a copy of the argument string. Unless an
	  * explicit copy of {@code original} is needed, use of this constructor is
	  * unnecessary since Strings are immutable.
	  *
	  * @param  original
	  * A { @code String}
	  */
	def this(original: String) {
		this()
		this.value = original.value
		this.hash = original.hash
	}
	
	/**
	  * Allocates a new {@code String} so that it represents the sequence of
	  * characters currently contained in the character array argument. The
	  * contents of the character array are copied; subsequent modification of
	  * the character array does not affect the newly created string.
	  *
	  * @param  value
	  * The initial value of the string
	  */
	def this(value: Array[Char]) {
		this()
		this.value = Arrays.copyOf(value, value.length)
	}
	
	/**
	  * Allocates a new {@code String} that contains characters from a subarray
	  * of the character array argument. The {@code offset} argument is the
	  * index of the first character of the subarray and the {@code count}
	  * argument specifies the length of the subarray. The contents of the
	  * subarray are copied; subsequent modification of the character array does
	  * not affect the newly created string.
	  *
	  * @param  value
	  * Array that is the source of characters
	  *
	  * @param  offset
	  * The initial offset
	  *
	  * @param  count
	  * The length
	  *
	  * @throws  IndexOutOfBoundsException
	  * If the { @code offset} and { @code count} arguments index
	  *                characters outside the bounds of the { @code value} array
	  */
	def this(value: Array[Char], offset: Int, count: Int)
	
	/**
	  * Allocates a new {@code String} that contains characters from a subarray
	  * of the <a href="Character.html#unicode">Unicode code point</a> array
	  * argument.  The {@code offset} argument is the index of the first code
	  * point of the subarray and the {@code count} argument specifies the
	  * length of the subarray.  The contents of the subarray are converted to
	  * {@code char}s; subsequent modification of the {@code int} array does not
	  * affect the newly created string.
	  *
	  * @param  codePoints
	  * Array that is the source of Unicode code points
	  *
	  * @param  offset
	  * The initial offset
	  *
	  * @param  count
	  * The length
	  *
	  * @throws  IllegalArgumentException
	  * If any invalid Unicode code point is found in { @code
	  * codePoints}
	  *
	  * @throws  IndexOutOfBoundsException
	  * If the { @code offset} and { @code count} arguments index
	  *                characters outside the bounds of the { @code codePoints} array
	  *
	  * @since 1.5
	  */
	def this(codePoints: Array[Int], offset: Int, count: Int)
	
	/**
	  * Allocates a new {@code String} constructed from a subarray of an array
	  * of 8-bit integer values.
	  *
	  * <p> The {@code offset} argument is the index of the first byte of the
	  * subarray, and the {@code count} argument specifies the length of the
	  * subarray.
	  *
	  * <p> Each {@code byte} in the subarray is converted to a {@code char} as
	  * specified in the method above.
	  *
	  * @deprecated This method does not properly convert bytes into characters.
	  *             As of JDK&nbsp;1.1, the preferred way to do this is via the
	  *             { @code String} constructors that take a { @link
	  * java.nio.charset.Charset}, charset name, or that use the platform's
	  * default charset.
	  *
	  * @param  ascii
	  * The bytes to be converted to characters
	  *
	  * @param  hibyte
	  * The top 8 bits of each 16-bit Unicode code unit
	  *
	  * @param  offset
	  * The initial offset
	  * @param  count
	  * The length
	  *
	  * @throws  IndexOutOfBoundsException
	  * If the { @code offset} or { @code count} argument is invalid
	  *
	  * @see #String(byte[], int)
	  * @see #String(byte[], int, int, java.lang.String)
	  * @see #String(byte[], int, int, java.nio.charset.Charset)
	  * @see #String(byte[], int, int)
	  * @see #String(byte[], java.lang.String)
	  * @see #String(byte[], java.nio.charset.Charset)
	  * @see #String(byte[])
	  */
	def this(ascii: Array[Byte], hibyte: Int, offset: Int, count: Int) {
		this()
		checkBounds(ascii, offset, count)
		val value: Array[Char] = new Array[Char](count)
		if (hibyte == 0) {
			var i: Int = count
			while ( { i -= 1; i + 1 } > 0) {
				{
					value(i) = (ascii(i + offset) & 0xff).toChar
				}
			}
		}
		else
		{
			hibyte <<= 8
			var i: Int = count
			while ( { i -= 1; i + 1 } > 0) {
				{
					value(i) = (hibyte | (ascii(i + offset) & 0xff)).toChar
				}
			}
		}
		this.value = value
	}
	
	/**
	  * Allocates a new {@code String} containing characters constructed from
	  * an array of 8-bit integer values. Each character <i>c</i>in the
	  * resulting string is constructed from the corresponding component
	  * <i>b</i> in the byte array such that:
	  *
	  * <blockquote><pre>
	  * <b><i>c</i></b> == (char)(((hibyte &amp; 0xff) &lt;&lt; 8)
	  * | (<b><i>b</i></b> &amp; 0xff))
	  * </pre></blockquote>
	  *
	  * @deprecated This method does not properly convert bytes into
	  * characters.  As of JDK&nbsp;1.1, the preferred way to do this is via the
	  *             { @code String} constructors that take a { @link
	  * java.nio.charset.Charset}, charset name, or that use the platform's
	  * default charset.
	  *
	  * @param  ascii
	  * The bytes to be converted to characters
	  *
	  * @param  hibyte
	  * The top 8 bits of each 16-bit Unicode code unit
	  *
	  * @see #String(byte[], int, int, java.lang.String)
	  * @see #String(byte[], int, int, java.nio.charset.Charset)
	  * @see #String(byte[], int, int)
	  * @see #String(byte[], java.lang.String)
	  * @see #String(byte[], java.nio.charset.Charset)
	  * @see #String(byte[])
	  */
	def this(ascii: Array[Byte], hibyte: Int) {
		this(ascii, hibyte, 0, ascii.length)
	}
	
	/* Common private utility method used to bounds check the byte array
     * and requested offset & length values used by the String(byte[],..)
     * constructors.
     */ private def checkBounds(bytes: Array[Byte], offset: Int, length: Int) {
		if (length < 0) throw new StringIndexOutOfBoundsException(length)
		if (offset < 0) throw new StringIndexOutOfBoundsException(offset)
		if (offset > bytes.length - length) throw new StringIndexOutOfBoundsException(offset + length)
	}
	
	/**
	  * Constructs a new {@code String} by decoding the specified subarray of
	  * bytes using the specified charset.  The length of the new {@code String}
	  * is a function of the charset, and hence may not be equal to the length
	  * of the subarray.
	  *
	  * <p> The behavior of this constructor when the given bytes are not valid
	  * in the given charset is unspecified.  The {@link
	  * java.nio.charset.CharsetDecoder} class should be used when more control
	  * over the decoding process is required.
	  *
	  * @param  bytes
	  * The bytes to be decoded into characters
	  *
	  * @param  offset
	  * The index of the first byte to decode
	  *
	  * @param  length
	  * The number of bytes to decode
	  *
	  * @param  charsetName
	  * The name of a supported { @linkplain
	}
	  *
	  * @throws  UnsupportedEncodingException
	  * If the named charset is not supported
	  *
	  * @throws  IndexOutOfBoundsException
	  * If the { @code offset} and { @code length} arguments index
	  *                characters outside the bounds of the { @code bytes} array
	  *
	  * @since JDK1.1
	  */
	def this(bytes: Array[Byte], offset: Int, length: Int, charsetName: String)
	
	/**
	  * Constructs a new {@code String} by decoding the specified subarray of
	  * bytes using the specified {@linkplain java.nio.charset.Charset charset}.
	  * The length of the new {@code String} is a function of the charset, and
	  * hence may not be equal to the length of the subarray.
	  *
	  * <p> This method always replaces malformed-input and unmappable-character
	  * sequences with this charset's default replacement string.  The {@link
	  * java.nio.charset.CharsetDecoder} class should be used when more control
	  * over the decoding process is required.
	  *
	  * @param  bytes
	  * The bytes to be decoded into characters
	  *
	  * @param  offset
	  * The index of the first byte to decode
	  *
	  * @param  length
	  * The number of bytes to decode
	  *
	  * @param  charset
	  * The { @linkplain java.nio.charset.Charset charset} to be used to
	  *                  decode the { @code bytes}
	  *
	  * @throws  IndexOutOfBoundsException
	  * If the { @code offset} and { @code length} arguments index
	  *                characters outside the bounds of the { @code bytes} array
	  *
	  * @since 1.6
	  */
	def this(bytes: Array[Byte], offset: Int, length: Int, charset: Charset)
	
	/**
	  * Constructs a new {@code String} by decoding the specified array of bytes
	  * using the specified {@linkplain java.nio.charset.Charset charset}.  The
	  * length of the new {@code String} is a function of the charset, and hence
	  * may not be equal to the length of the byte array.
	  *
	  * <p> The behavior of this constructor when the given bytes are not valid
	  * in the given charset is unspecified.  The {@link
	  * java.nio.charset.CharsetDecoder} class should be used when more control
	  * over the decoding process is required.
	  *
	  * @param  bytes
	  * The bytes to be decoded into characters
	  *
	  * @param  charsetName
	  * The name of a supported { @linkplain
	}
	  *
	  * @throws  UnsupportedEncodingException
	  * If the named charset is not supported
	  *
	  * @since JDK1.1
	  */
	def this(bytes: Array[Byte], charsetName: String) {
		this(bytes, 0, bytes.length, charsetName)
	}
	
	/**
	  * Constructs a new {@code String} by decoding the specified array of
	  * bytes using the specified {@linkplain java.nio.charset.Charset charset}.
	  * The length of the new {@code String} is a function of the charset, and
	  * hence may not be equal to the length of the byte array.
	  *
	  * <p> This method always replaces malformed-input and unmappable-character
	  * sequences with this charset's default replacement string.  The {@link
	  * java.nio.charset.CharsetDecoder} class should be used when more control
	  * over the decoding process is required.
	  *
	  * @param  bytes
	  * The bytes to be decoded into characters
	  *
	  * @param  charset
	  * The { @linkplain java.nio.charset.Charset charset} to be used to
	  *                  decode the { @code bytes}
	  *
	  * @since 1.6
	  */
	def this(bytes: Array[Byte], charset: Charset) {
		this(bytes, 0, bytes.length, charset)
	}
	
	/**
	  * Constructs a new {@code String} by decoding the specified subarray of
	  * bytes using the platform's default charset.  The length of the new
	  * {@code String} is a function of the charset, and hence may not be equal
	  * to the length of the subarray.
	  *
	  * <p> The behavior of this constructor when the given bytes are not valid
	  * in the default charset is unspecified.  The {@link
	  * java.nio.charset.CharsetDecoder} class should be used when more control
	  * over the decoding process is required.
	  *
	  * @param  bytes
	  * The bytes to be decoded into characters
	  *
	  * @param  offset
	  * The index of the first byte to decode
	  *
	  * @param  length
	  * The number of bytes to decode
	  *
	  * @throws  IndexOutOfBoundsException
	  * If the { @code offset} and the { @code length} arguments index
	  *                characters outside the bounds of the { @code bytes} array
	  *
	  * @since JDK1.1
	  */
	def this(bytes: Array[Byte], offset: Int, length: Int) {
		this()
		checkBounds(bytes, offset, length)
		this.value = StringCoding.decode(bytes, offset, length)
	}
	
	/**
	  * Constructs a new {@code String} by decoding the specified array of bytes
	  * using the platform's default charset.  The length of the new {@code
	  * String} is a function of the charset, and hence may not be equal to the
	  * length of the byte array.
	  *
	  * <p> The behavior of this constructor when the given bytes are not valid
	  * in the default charset is unspecified.  The {@link
	  * java.nio.charset.CharsetDecoder} class should be used when more control
	  * over the decoding process is required.
	  *
	  * @param  bytes
	  * The bytes to be decoded into characters
	  *
	  * @since JDK1.1
	  */
	def this(bytes: Array[Byte]) {
		this(bytes, 0, bytes.length)
	}
	
	/**
	  * Allocates a new string that contains the sequence of characters
	  * currently contained in the string buffer argument. The contents of the
	  * string buffer are copied; subsequent modification of the string buffer
	  * does not affect the newly created string.
	  *
	  * @param  buffer
	  * A { @code StringBuffer}
	  */
	def this(buffer: StringBuffer)
	
	/**
	  * Allocates a new string that contains the sequence of characters
	  * currently contained in the string builder argument. The contents of the
	  * string builder are copied; subsequent modification of the string builder
	  * does not affect the newly created string.
	  *
	  * <p> This constructor is provided to ease migration to {@code
	  * StringBuilder}. Obtaining a string from a string builder via the {@code
	  * toString} method is likely to run faster and is generally preferred.
	  *
	  * @param   builder
	  * A { @code StringBuilder}
	  *
	  * @since 1.5
	  */
	def this(builder: StringBuilder) {
		this()
		this.value = Arrays.copyOf(builder.getValue, builder.length)
	}
	
	/*
    * Package private constructor which shares value array for speed.
    * this constructor is always expected to be called with share==true.
    * a separate constructor is needed because we already have a public
    * String(char[]) constructor that makes a copy of the given char[].
    */ def this(value: Array[Char], share: Boolean) {
		this()
		// assert share : "unshared not supported";
		this.value = value
	}
	
	/**
	  * Returns the length of this string.
	  * The length is equal to the number of <a href="Character.html#unicode">Unicode
	  * code units</a> in the string.
	  *
	  * @return the length of the sequence of characters represented by this
	  *         object.
	  */
	def length: Int = {
		return value.length
	}
	
	/**
	  * Returns {@code true} if, and only if, {@link #length()} is {@code 0}.
	  *
	  * @return { @code true} if { @link #length()} is { @code 0}, otherwise
	  *                                                        { @code false}
	  *
	  * @since 1.6
	  */
	def isEmpty: Boolean = {
		return value.length == 0
	}
	
	/**
	  * Returns the {@code char} value at the
	  * specified index. An index ranges from {@code 0} to
	  * {@code length() - 1}. The first {@code char} value of the sequence
	  * is at index {@code 0}, the next at index {@code 1},
	  * and so on, as for array indexing.
	  *
	  * <p>If the {@code char} value specified by the index is a
	  * <a href="Character.html#unicode">surrogate</a>, the surrogate
	  * value is returned.
	  *
	  * @param      index the index of the { @code char} value.
	  * @return the { @code char} value at the specified index of this string.
	  *                     The first { @code char} value is at index { @code 0}.
	  * @exception IndexOutOfBoundsException  if the { @code index}
	  *                                                      argument is negative or not less than the length of this
	  *                                                      string.
	  */
	def charAt(index: Int): Char = {
		if ((index < 0) || (index >= value.length)) {
			throw new StringIndexOutOfBoundsException(index)
		}
		return value(index)
	}
	
	/**
	  * Returns the character (Unicode code point) at the specified
	  * index. The index refers to {@code char} values
	  * (Unicode code units) and ranges from {@code 0} to
	  * {@link #length()}
	{@code - 1}.
	  *
	  * <p> If the {@code char} value specified at the given index
	  * is in the high-surrogate range, the following index is less
	  * than the length of this {@code String}, and the
	  * {@code char} value at the following index is in the
	  * low-surrogate range, then the supplementary code point
	  * corresponding to this surrogate pair is returned. Otherwise,
	  * the {@code char} value at the given index is returned.
	  *
	  * @param      index the index to the { @code char} values
	  * @return the code point value of the character at the
	  *         { @code index}
	  * @exception IndexOutOfBoundsException  if the { @code index}
	  *                                                      argument is negative or not less than the length of this
	  *                                                      string.
	  * @since 1.5
	  */
	def codePointAt(index: Int): Int = {
		if ((index < 0) || (index >= value.length)) {
			throw new StringIndexOutOfBoundsException(index)
		}
		return Character.codePointAtImpl(value, index, value.length)
	}
	
	/**
	  * Returns the character (Unicode code point) before the specified
	  * index. The index refers to {@code char} values
	  * (Unicode code units) and ranges from {@code 1} to {@link
	  * CharSequence#length() length}.
	  *
	  * <p> If the {@code char} value at {@code (index - 1)}
	  * is in the low-surrogate range, {@code (index - 2)} is not
	  * negative, and the {@code char} value at {@code (index -
     * 2)} is in the high-surrogate range, then the
	  * supplementary code point value of the surrogate pair is
	  * returned. If the {@code char} value at {@code index -
     * 1} is an unpaired low-surrogate or a high-surrogate, the
	  * surrogate value is returned.
	  *
	  * @param     index the index following the code point that should be returned
	  * @return the Unicode code point value before the given index.
	  * @exception IndexOutOfBoundsException if the { @code index}
	  *                                                     argument is less than 1 or greater than the length
	  *                                                     of this string.
	  * @since 1.5
	  */
	def codePointBefore(index: Int): Int = {
		val i: Int = index - 1
		if ((i < 0) || (i >= value.length)) {
			throw new StringIndexOutOfBoundsException(index)
		}
		return Character.codePointBeforeImpl(value, index, 0)
	}
	
	/**
	  * Returns the number of Unicode code points in the specified text
	  * range of this {@code String}. The text range begins at the
	  * specified {@code beginIndex} and extends to the
	  * {@code char} at index {@code endIndex - 1}. Thus the
	  * length (in {@code char}s) of the text range is
	  * {@code endIndex-beginIndex}. Unpaired surrogates within
	  * the text range count as one code point each.
	  *
	  * @param beginIndex the index to the first { @code char} of
	  *                                                  the text range.
	  * @param endIndex the index after the last { @code char} of
	  *                                                  the text range.
	  * @return the number of Unicode code points in the specified text
	  *         range
	  * @exception IndexOutOfBoundsException if the
	  *            { @code beginIndex} is negative, or { @code endIndex}
	  *                    is larger than the length of this { @code String}, or
	  *                    { @code beginIndex} is larger than { @code endIndex}.
	  * @since 1.5
	  */
	def codePointCount(beginIndex: Int, endIndex: Int): Int = {
		if (beginIndex < 0 || endIndex > value.length || beginIndex > endIndex) {
			throw new IndexOutOfBoundsException
		}
		return Character.codePointCountImpl(value, beginIndex, endIndex - beginIndex)
	}
	
	/**
	  * Returns the index within this {@code String} that is
	  * offset from the given {@code index} by
	  * {@code codePointOffset} code points. Unpaired surrogates
	  * within the text range given by {@code index} and
	  * {@code codePointOffset} count as one code point each.
	  *
	  * @param index           the index to be offset
	  * @param codePointOffset the offset in code points
	  * @return the index within this { @code String}
	  * @exception IndexOutOfBoundsException if { @code index}
	  *                                                 is negative or larger then the length of this
	  *                                                 { @code String}, or if { @code codePointOffset} is positive
	  *                                                 and the substring starting with { @code index} has fewer
	  *                                                 than { @code codePointOffset} code points,
	  *                                                 or if { @code codePointOffset} is negative and the substring
	  *                                                 before { @code index} has fewer than the absolute value
	  *                                                 of { @code codePointOffset} code points.
	  * @since 1.5
	  */
	def offsetByCodePoints(index: Int, codePointOffset: Int): Int = {
		if (index < 0 || index > value.length) {
			throw new IndexOutOfBoundsException
		}
		return Character.offsetByCodePointsImpl(value, 0, value.length, index, codePointOffset)
	}
	
	/**
	  * Copy characters from this string into dst starting at dstBegin.
	  * This method doesn't perform any range checking.
	  */
	private[lang] def getChars(dst: Array[Char], dstBegin: Int) {
		System.arraycopy(value, 0, dst, dstBegin, value.length)
	}
	
	/**
	  * Copies characters from this string into the destination character
	  * array.
	  * <p>
	  * The first character to be copied is at index {@code srcBegin};
	  * the last character to be copied is at index {@code srcEnd-1}
	  * (thus the total number of characters to be copied is
	  * {@code srcEnd-srcBegin}). The characters are copied into the
	  * subarray of {@code dst} starting at index {@code dstBegin}
	  * and ending at index:
	  * <blockquote><pre>
	  * dstBegin + (srcEnd-srcBegin) - 1
	  * </pre></blockquote>
	  *
	  * @param      srcBegin index of the first character in the string
	  *                      to copy.
	  * @param      srcEnd   index after the last character in the string
	  *                      to copy.
	  * @param      dst      the destination array.
	  * @param      dstBegin the start offset in the destination array.
	  * @exception IndexOutOfBoundsException If any of the following
	  *            is true:
	  *            <ul><li>{ @code srcBegin} is negative.
	  *                            <li>{ @code srcBegin} is greater than { @code srcEnd}
	  *                            <li>{ @code srcEnd} is greater than the length of this
	  *                            string
	  *                            <li>{ @code dstBegin} is negative
	  *                            <li>{ @code dstBegin+(srcEnd-srcBegin)} is larger than
	  *                            { @code dst.length}</ul>
	  */
	def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Char], dstBegin: Int) {
		if (srcBegin < 0) {
			throw new StringIndexOutOfBoundsException(srcBegin)
		}
		if (srcEnd > value.length) {
			throw new StringIndexOutOfBoundsException(srcEnd)
		}
		if (srcBegin > srcEnd) {
			throw new StringIndexOutOfBoundsException(srcEnd - srcBegin)
		}
		System.arraycopy(value, srcBegin, dst, dstBegin, srcEnd - srcBegin)
	}
	
	/**
	  * Copies characters from this string into the destination byte array. Each
	  * byte receives the 8 low-order bits of the corresponding character. The
	  * eight high-order bits of each character are not copied and do not
	  * participate in the transfer in any way.
	  *
	  * <p> The first character to be copied is at index {@code srcBegin}; the
	  * last character to be copied is at index {@code srcEnd-1}.  The total
	  * number of characters to be copied is {@code srcEnd-srcBegin}. The
	  * characters, converted to bytes, are copied into the subarray of {@code
	  * dst} starting at index {@code dstBegin} and ending at index:
	  *
	  * <blockquote><pre>
	  * dstBegin + (srcEnd-srcBegin) - 1
	  * </pre></blockquote>
	  *
	  * @deprecated This method does not properly convert characters into
	  * bytes.  As of JDK&nbsp;1.1, the preferred way to do this is via the
	  *             { @link #getBytes()} method, which uses the platform's default charset.
	  *
	  * @param  srcBegin
	  * Index of the first character in the string to copy
	  *
	  * @param  srcEnd
	  * Index after the last character in the string to copy
	  *
	  * @param  dst
	  * The destination array
	  *
	  * @param  dstBegin
	  * The start offset in the destination array
	  *
	  * @throws  IndexOutOfBoundsException
	  * If any of the following is true:
	  * <ul>
	  * <li> { @code    srcBegin} is negative
	  *                 <li> { @code srcBegin} is greater than { @code srcEnd}
	  *                 <li> { @code srcEnd} is greater than the length of this String
	  *                 <li> { @code dstBegin} is negative
	  *                 <li> { @code dstBegin+(srcEnd-srcBegin)} is larger than { @code
	  *                 dst.length}
	  *                 </ul>
	  */
	@deprecated def getBytes(srcBegin: Int, srcEnd: Int, dst: Array[Byte], dstBegin: Int) {
		if (srcBegin < 0) {
			throw new StringIndexOutOfBoundsException(srcBegin)
		}
		if (srcEnd > value.length) {
			throw new StringIndexOutOfBoundsException(srcEnd)
		}
		if (srcBegin > srcEnd) {
			throw new StringIndexOutOfBoundsException(srcEnd - srcBegin)
		}
		Objects.requireNonNull(dst)
		var j: Int = dstBegin
		val n: Int = srcEnd
		var i: Int = srcBegin
		val `val`: Array[Char] = value /* avoid getfield opcode */
		while (i < n) {
			{
				dst({ j += 1; j - 1 }) = `val`({ i += 1; i - 1 }).toByte
			}
		}
	}
	
	/**
	  * Encodes this {@code String} into a sequence of bytes using the named
	  * charset, storing the result into a new byte array.
	  *
	  * <p> The behavior of this method when this string cannot be encoded in
	  * the given charset is unspecified.  The {@link
	  * java.nio.charset.CharsetEncoder} class should be used when more control
	  * over the encoding process is required.
	  *
	  * @param  charsetName
	  * The name of a supported { @linkplain
	}
	  *
	  * @return The resultant byte array
	  *
	  * @throws  UnsupportedEncodingException
	  * If the named charset is not supported
	  *
	  * @since JDK1.1
	  */
	@throws[UnsupportedEncodingException]
	def getBytes(charsetName: String): Array[Byte] = {
		if (charsetName == null) throw new NullPointerException
		return StringCoding.encode(charsetName, value, 0, value.length)
	}
	
	/**
	  * Encodes this {@code String} into a sequence of bytes using the given
	  * {@linkplain java.nio.charset.Charset charset}, storing the result into a
	  * new byte array.
	  *
	  * <p> This method always replaces malformed-input and unmappable-character
	  * sequences with this charset's default replacement byte array.  The
	  * {@link java.nio.charset.CharsetEncoder} class should be used when more
	  * control over the encoding process is required.
	  *
	  * @param  charset
	  * The { @linkplain java.nio.charset.Charset} to be used to encode
	  *                  the { @code String}
	  *
	  * @return The resultant byte array
	  *
	  * @since 1.6
	  */
	def getBytes(charset: Charset): Array[Byte] = {
		if (charset == null) throw new NullPointerException
		return StringCoding.encode(charset, value, 0, value.length)
	}
	
	/**
	  * Encodes this {@code String} into a sequence of bytes using the
	  * platform's default charset, storing the result into a new byte array.
	  *
	  * <p> The behavior of this method when this string cannot be encoded in
	  * the default charset is unspecified.  The {@link
	  * java.nio.charset.CharsetEncoder} class should be used when more control
	  * over the encoding process is required.
	  *
	  * @return The resultant byte array
	  *
	  * @since JDK1.1
	  */
	def getBytes: Array[Byte] = {
		return StringCoding.encode(value, 0, value.length)
	}
	
	/**
	  * Compares this string to the specified object.  The result is {@code
	  * true} if and only if the argument is not {@code null} and is a {@code
	  * String} object that represents the same sequence of characters as this
	  * object.
	  *
	  * @param  anObject
	  * The object to compare this { @code String} against
	  *
	  * @return { @code true} if the given object represents a { @code String}
	  *                 equivalent to this string, { @code false} otherwise
	  *
	  * @see #compareTo(String)
	  * @see #equalsIgnoreCase(String)
	  */
	override def equals(anObject: Any): Boolean = {
		if (this eq anObject) {
			return true
		}
		if (anObject.isInstanceOf[String]) {
			val anotherString: String = anObject.asInstanceOf[String]
			var n: Int = value.length
			if (n == anotherString.value.length) {
				val v1: Array[Char] = value
				val v2: Array[Char] = anotherString.value
				var i: Int = 0
				while ( { n -= 1; n + 1 } != 0) {
					{
						if (v1(i) != v2(i)) return false
						i += 1
					}
				}
				return true
			}
		}
		return false
	}
	
	/**
	  * Compares this string to the specified {@code StringBuffer}.  The result
	  * is {@code true} if and only if this {@code String} represents the same
	  * sequence of characters as the specified {@code StringBuffer}. This method
	  * synchronizes on the {@code StringBuffer}.
	  *
	  * @param  sb
	  * The { @code StringBuffer} to compare this { @code String} against
	  *
	  * @return { @code true} if this { @code String} represents the same
	  *                 sequence of characters as the specified { @code StringBuffer},
	  *                 { @code false} otherwise
	  *
	  * @since 1.4
	  */
	def contentEquals(sb: StringBuffer): Boolean = {
		return contentEquals(sb.asInstanceOf[CharSequence])
	}
	
	private def nonSyncContentEquals(sb: AbstractStringBuilder): Boolean = {
		val v1: Array[Char] = value
		val v2: Array[Char] = sb.getValue
		val n: Int = v1.length
		if (n != sb.length) {
			return false
		}
		var i: Int = 0
		while (i < n) {
			{
				if (v1(i) != v2(i)) {
					return false
				}
			}
			{i += 1; i - 1 }
		}
		return true
	}
	
	/**
	  * Compares this string to the specified {@code CharSequence}.  The
	  * result is {@code true} if and only if this {@code String} represents the
	  * same sequence of char values as the specified sequence. Note that if the
	  * {@code CharSequence} is a {@code StringBuffer} then the method
	  * synchronizes on it.
	  *
	  * @param  cs
	  * The sequence to compare this { @code String} against
	  *
	  * @return { @code true} if this { @code String} represents the same
	  *                 sequence of char values as the specified sequence, { @code
	  *                 false} otherwise
	  *
	  * @since 1.5
	  */
	def contentEquals(cs: CharSequence): Boolean = {
		// Argument is a StringBuffer, StringBuilder
		if (cs.isInstanceOf[AbstractStringBuilder]) {
			if (cs.isInstanceOf[StringBuffer]) {
				cs synchronized {
					return nonSyncContentEquals(cs.asInstanceOf[AbstractStringBuilder])
				}
			}
			else
			{
				return nonSyncContentEquals(cs.asInstanceOf[AbstractStringBuilder])
			}
		}
		// Argument is a String
		if (cs.isInstanceOf[String]) {
			return this == cs
		}
		// Argument is a generic CharSequence
		val v1: Array[Char] = value
		val n: Int = v1.length
		if (n != cs.length) {
			return false
		}
		var i: Int = 0
		while (i < n) {
			{
				if (v1(i) != cs.charAt(i)) {
					return false
				}
			}
			{i += 1; i - 1 }
		}
		return true
	}
	
	/**
	  * Compares this {@code String} to another {@code String}, ignoring case
	  * considerations.  Two strings are considered equal ignoring case if they
	  * are of the same length and corresponding characters in the two strings
	  * are equal ignoring case.
	  *
	  * <p> Two characters {@code c1} and {@code c2} are considered the same
	  * ignoring case if at least one of the following is true:
	  * <ul>
	  * <li> The two characters are the same (as compared by the
	  * {@code ==} operator)
	  * <li> Applying the method {@link
	  *        java.lang.Character#toUpperCase(char)} to each character
	  * produces the same result
	  * <li> Applying the method {@link
	  *        java.lang.Character#toLowerCase(char)} to each character
	  * produces the same result
	  * </ul>
	  *
	  * @param  anotherString
	  * The { @code String} to compare this { @code String} against
	  *
	  * @return { @code true} if the argument is not { @code null} and it
	  *                 represents an equivalent { @code String} ignoring case; { @code
	  *                 false} otherwise
	  *
	  * @see #equals(Object)
	  */
	def equalsIgnoreCase(anotherString: String): Boolean = {
		return if (((this eq anotherString))) true
		       else (anotherString != null) && (anotherString.value.length == value.length) && regionMatches(true, 0,
			       anotherString, 0, value.length)
	}
	
	/**
	  * Compares two strings lexicographically.
	  * The comparison is based on the Unicode value of each character in
	  * the strings. The character sequence represented by this
	  * {@code String} object is compared lexicographically to the
	  * character sequence represented by the argument string. The result is
	  * a negative integer if this {@code String} object
	  * lexicographically precedes the argument string. The result is a
	  * positive integer if this {@code String} object lexicographically
	  * follows the argument string. The result is zero if the strings
	  * are equal; {@code compareTo} returns {@code 0} exactly when
	  * the {@link #equals(Object)} method would return {@code true}.
	  * <p>
	  * This is the definition of lexicographic ordering. If two strings are
	  * different, then either they have different characters at some index
	  * that is a valid index for both strings, or their lengths are different,
	  * or both. If they have different characters at one or more index
	  * positions, let <i>k</i> be the smallest such index; then the string
	  * whose character at position <i>k</i> has the smaller value, as
	  * determined by using the &lt; operator, lexicographically precedes the
	  * other string. In this case, {@code compareTo} returns the
	  * difference of the two character values at position {@code k} in
	  * the two string -- that is, the value:
	  * <blockquote><pre>
	  * this.charAt(k)-anotherString.charAt(k)
	  * </pre></blockquote>
	  * If there is no index position at which they differ, then the shorter
	  * string lexicographically precedes the longer string. In this case,
	  * {@code compareTo} returns the difference of the lengths of the
	  * strings -- that is, the value:
	  * <blockquote><pre>
	  * this.length()-anotherString.length()
	  * </pre></blockquote>
	  *
	  * @param   anotherString the { @code String} to be compared.
	  * @return the value { @code 0} if the argument string is equal to
	  *                           this string; a value less than { @code 0} if this string
	  *                           is lexicographically less than the string argument; and a
	  *                           value greater than { @code 0} if this string is
	  *                           lexicographically greater than the string argument.
	  */
	def compareTo(anotherString: String): Int = {
		val len1: Int = value.length
		val len2: Int = anotherString.value.length
		val lim: Int = Math.min(len1, len2)
		val v1: Array[Char] = value
		val v2: Array[Char] = anotherString.value
		var k: Int = 0
		while (k < lim) {
			{
				val c1: Char = v1(k)
				val c2: Char = v2(k)
				if (c1 != c2) {
					return c1 - c2
				}
				k += 1
			}
		}
		return len1 - len2
	}
	
	/**
	  * A Comparator that orders {@code String} objects as by
	  * {@code compareToIgnoreCase}. This comparator is serializable.
	  * <p>
	  * Note that this Comparator does <em>not</em> take locale into account,
	  * and will result in an unsatisfactory ordering for certain locales.
	  * The java.text package provides <em>Collators</em> to allow
	  * locale-sensitive ordering.
	  *
	  * @see java.text.Collator#compare(String, String)
	  * @since 1.2
	  */
	val CASE_INSENSITIVE_ORDER: Comparator[String] = new String.CaseInsensitiveComparator
	
	@SerialVersionUID(8575799808933029326L)
	private class CaseInsensitiveComparator extends Comparator[String] with java.io.Serializable {
		def compare(s1: String, s2: String): Int = {
			val n1: Int = s1.length
			val n2: Int = s2.length
			val min: Int = Math.min(n1, n2)
			var i: Int = 0
			while (i < min) {
				{
					var c1: Char = s1.charAt(i)
					var c2: Char = s2.charAt(i)
					if (c1 != c2) {
						c1 = Character.toUpperCase(c1)
						c2 = Character.toUpperCase(c2)
						if (c1 != c2) {
							c1 = Character.toLowerCase(c1)
							c2 = Character.toLowerCase(c2)
							if (c1 != c2) {
								// No overflow because of numeric promotion
								return c1 - c2
							}
						}
					}
				}
				{i += 1; i - 1 }
			}
			return n1 - n2
		}
		
		/** Replaces the de-serialized object. */
		private def readResolve: Any = {
			return CASE_INSENSITIVE_ORDER
		}
	}
	
	/**
	  * Compares two strings lexicographically, ignoring case
	  * differences. This method returns an integer whose sign is that of
	  * calling {@code compareTo} with normalized versions of the strings
	  * where case differences have been eliminated by calling
	  * {@code Character.toLowerCase(Character.toUpperCase(character))} on
	  * each character.
	  * <p>
	  * Note that this method does <em>not</em> take locale into account,
	  * and will result in an unsatisfactory ordering for certain locales.
	  * The java.text package provides <em>collators</em> to allow
	  * locale-sensitive ordering.
	  *
	  * @param   str the { @code String} to be compared.
	  * @return a negative integer, zero, or a positive integer as the
	  *         specified String is greater than, equal to, or less
	  *         than this String, ignoring case considerations.
	  * @see java.text.Collator#compare(String, String)
	  * @since 1.2
	  */
	def compareToIgnoreCase(str: String): Int = {
		return CASE_INSENSITIVE_ORDER.compare(this, str)
	}
	
	/**
	  * Tests if two string regions are equal.
	  * <p>
	  * A substring of this {@code String} object is compared to a substring
	  * of the argument other. The result is true if these substrings
	  * represent identical character sequences. The substring of this
	  * {@code String} object to be compared begins at index {@code toffset}
	  * and has length {@code len}. The substring of other to be compared
	  * begins at index {@code ooffset} and has length {@code len}. The
	  * result is {@code false} if and only if at least one of the following
	  * is true:
	  * <ul><li>{@code toffset} is negative.
	  * <li>{@code ooffset} is negative.
	  * <li>{@code toffset+len} is greater than the length of this
	  * {@code String} object.
	  * <li>{@code ooffset+len} is greater than the length of the other
	  * argument.
	  * <li>There is some nonnegative integer <i>k</i> less than {@code len}
	  * such that:
	  * {@code this.charAt(toffset + }<i>k</i>{@code ) != other.charAt(ooffset + }
	  * <i>k</i>{@code )}
	  * </ul>
	  *
	  * @param   toffset the starting offset of the subregion in this string.
	  * @param   other   the string argument.
	  * @param   ooffset the starting offset of the subregion in the string
	  *                  argument.
	  * @param   len     the number of characters to compare.
	  * @return { @code true} if the specified subregion of this string
	  *                 exactly matches the specified subregion of the string argument;
	  *                 { @code false} otherwise.
	  */
	def regionMatches(toffset: Int, other: String, ooffset: Int, len: Int): Boolean = {
		val ta: Array[Char] = value
		var to: Int = toffset
		val pa: Array[Char] = other.value
		var po: Int = ooffset
		// Note: toffset, ooffset, or len might be near -1>>>1.
		if ((ooffset < 0) || (toffset < 0) || (toffset > value.length.toLong - len) || (ooffset > other.value.length
			.toLong - len))
		{
			return false
		}
		while ( { len -= 1; len + 1 } > 0) {
			{
				if (ta({ to += 1; to - 1 }) != pa({ po += 1; po - 1 })) {
					return false
				}
			}
		}
		return true
	}
	
	/**
	  * Tests if two string regions are equal.
	  * <p>
	  * A substring of this {@code String} object is compared to a substring
	  * of the argument {@code other}. The result is {@code true} if these
	  * substrings represent character sequences that are the same, ignoring
	  * case if and only if {@code ignoreCase} is true. The substring of
	  * this {@code String} object to be compared begins at index
	  * {@code toffset} and has length {@code len}. The substring of
	  * {@code other} to be compared begins at index {@code ooffset} and
	  * has length {@code len}. The result is {@code false} if and only if
	  * at least one of the following is true:
	  * <ul><li>{@code toffset} is negative.
	  * <li>{@code ooffset} is negative.
	  * <li>{@code toffset+len} is greater than the length of this
	  * {@code String} object.
	  * <li>{@code ooffset+len} is greater than the length of the other
	  * argument.
	  * <li>{@code ignoreCase} is {@code false} and there is some nonnegative
	  * integer <i>k</i> less than {@code len} such that:
	  * <blockquote><pre>
	  * this.charAt(toffset+k) != other.charAt(ooffset+k)
	  * </pre></blockquote>
	  * <li>{@code ignoreCase} is {@code true} and there is some nonnegative
	  * integer <i>k</i> less than {@code len} such that:
	  * <blockquote><pre>
	  * Character.toLowerCase(this.charAt(toffset+k)) !=
	  *Character.toLowerCase(other.charAt(ooffset+k))
	  * </pre></blockquote>
	  * and:
	  * <blockquote><pre>
	  * Character.toUpperCase(this.charAt(toffset+k)) !=
	  *         Character.toUpperCase(other.charAt(ooffset+k))
	  * </pre></blockquote>
	  * </ul>
	  *
	  * @param   ignoreCase if { @code true}, ignore case when comparing
	  *                                characters.
	  * @param   toffset the starting offset of the subregion in this
	  *                  string.
	  * @param   other   the string argument.
	  * @param   ooffset the starting offset of the subregion in the string
	  *                  argument.
	  * @param   len     the number of characters to compare.
	  * @return { @code true} if the specified subregion of this string
	  *                 matches the specified subregion of the string argument;
	  *                 { @code false} otherwise. Whether the matching is exact
	  *                 or case insensitive depends on the { @code ignoreCase}
	  *                 argument.
	  */
	def regionMatches(ignoreCase: Boolean, toffset: Int, other: String, ooffset: Int, len: Int): Boolean = {
		val ta: Array[Char] = value
		var to: Int = toffset
		val pa: Array[Char] = other.value
		var po: Int = ooffset
		// Note: toffset, ooffset, or len might be near -1>>>1.
		if ((ooffset < 0) || (toffset < 0) || (toffset > value.length.toLong - len) || (ooffset > other.value.length
			.toLong - len))
		{
			return false
		}
		while ( { len -= 1; len + 1 } > 0) {
			{
				val c1: Char = ta({ to += 1; to - 1 })
				val c2: Char = pa({ po += 1; po - 1 })
				if (c1 == c2) {
					continue //todo: continue is not supported
				}
				if (ignoreCase) {
					// If characters don't match but case may be ignored,
					// try converting both characters to uppercase.
					// If the results match, then the comparison scan should
					// continue.
					val u1: Char = Character.toUpperCase(c1)
					val u2: Char = Character.toUpperCase(c2)
					if (u1 == u2) {
						continue //todo: continue is not supported
					}
					// Unfortunately, conversion to uppercase does not work properly
					// for the Georgian alphabet, which has strange rules about case
					// conversion.  So we need to make one last check before
					// exiting.
					if (Character.toLowerCase(u1) == Character.toLowerCase(u2)) {
						continue //todo: continue is not supported
					}
				}
				return false
			}
		}
		return true
	}
	
	/**
	  * Tests if the substring of this string beginning at the
	  * specified index starts with the specified prefix.
	  *
	  * @param   prefix  the prefix.
	  * @param   toffset where to begin looking in this string.
	  * @return { @code true} if the character sequence represented by the
	  *                 argument is a prefix of the substring of this object starting
	  *                 at index { @code toffset}; { @code false} otherwise.
	  *                 The result is { @code false} if { @code toffset} is
	  *                 negative or greater than the length of this
	  *                 { @code String} object; otherwise the result is the same
	  *                 as the result of the expression
	  *                 <pre>
	  *          this.substring(toffset).startsWith(prefix)
	  *                 </pre>
	  */
	def startsWith(prefix: String, toffset: Int): Boolean = {
		val ta: Array[Char] = value
		var to: Int = toffset
		val pa: Array[Char] = prefix.value
		var po: Int = 0
		var pc: Int = prefix.value.length
		// Note: toffset might be near -1>>>1.
		if ((toffset < 0) || (toffset > value.length - pc)) {
			return false
		}
		while ( { pc -= 1; pc } >= 0) {
			{
				if (ta({ to += 1; to - 1 }) != pa({ po += 1; po - 1 })) {
					return false
				}
			}
		}
		return true
	}
	
	/**
	  * Tests if this string starts with the specified prefix.
	  *
	  * @param   prefix the prefix.
	  * @return { @code true} if the character sequence represented by the
	  *                 argument is a prefix of the character sequence represented by
	  *                 this string; { @code false} otherwise.
	  *                 Note also that { @code true} will be returned if the
	  *                 argument is an empty string or is equal to this
	  *                 { @code String} object as determined by the
	  *                 { @link #equals(Object)} method.
	  * @since 1. 0
	  */
	def startsWith(prefix: String): Boolean = {
		return startsWith(prefix, 0)
	}
	
	/**
	  * Tests if this string ends with the specified suffix.
	  *
	  * @param   suffix the suffix.
	  * @return { @code true} if the character sequence represented by the
	  *                 argument is a suffix of the character sequence represented by
	  *                 this object; { @code false} otherwise. Note that the
	  *                 result will be { @code true} if the argument is the
	  *                 empty string or is equal to this { @code String} object
	  *                 as determined by the { @link #equals(Object)} method.
	  */
	def endsWith(suffix: String): Boolean = {
		return startsWith(suffix, value.length - suffix.value.length)
	}
	
	/**
	  * Returns a hash code for this string. The hash code for a
	  * {@code String} object is computed as
	  * <blockquote><pre>
	  * s[0]*31^(n-1) + s[1]*31^(n-2) + ... + s[n-1]
	  * </pre></blockquote>
	  * using {@code int} arithmetic, where {@code s[i]} is the
	  * <i>i</i>th character of the string, {@code n} is the length of
	  * the string, and {@code ^} indicates exponentiation.
	  * (The hash value of the empty string is zero.)
	  *
	  * @return a hash code value for this object.
	  */
	override def hashCode: Int = {
		var h: Int = hash
		if (h == 0 && value.length > 0) {
			val `val`: Array[Char] = value
			var i: Int = 0
			while (i < value.length) {
				{
					h = 31 * h + `val`(i)
				}
				{i += 1; i - 1 }
			}
			hash = h
		}
		return h
	}
	
	/**
	  * Returns the index within this string of the first occurrence of
	  * the specified character. If a character with value
	  * {@code ch} occurs in the character sequence represented by
	  * this {@code String} object, then the index (in Unicode
	  * code units) of the first such occurrence is returned. For
	  * values of {@code ch} in the range from 0 to 0xFFFF
	  * (inclusive), this is the smallest value <i>k</i> such that:
	  * <blockquote><pre>
	  * this.charAt(<i>k</i>) == ch
	  * </pre></blockquote>
	  * is true. For other values of {@code ch}, it is the
	  * smallest value <i>k</i> such that:
	  * <blockquote><pre>
	  * this.codePointAt(<i>k</i>) == ch
	  * </pre></blockquote>
	  * is true. In either case, if no such character occurs in this
	  * string, then {@code -1} is returned.
	  *
	  * @param   ch a character (Unicode code point).
	  * @return the index of the first occurrence of the character in the
	  *         character sequence represented by this object, or
	  *         { @code -1} if the character does not occur.
	  */
	def indexOf(ch: Int): Int = {
		return indexOf(ch, 0)
	}
	
	/**
	  * Returns the index within this string of the first occurrence of the
	  * specified character, starting the search at the specified index.
	  * <p>
	  * If a character with value {@code ch} occurs in the
	  * character sequence represented by this {@code String}
	  * object at an index no smaller than {@code fromIndex}, then
	  * the index of the first such occurrence is returned. For values
	  * of {@code ch} in the range from 0 to 0xFFFF (inclusive),
	  * this is the smallest value <i>k</i> such that:
	  * <blockquote><pre>
	  * (this.charAt(<i>k</i>) == ch) {@code &&} (<i>k</i> &gt;= fromIndex)
	  * </pre></blockquote>
	  * is true. For other values of {@code ch}, it is the
	  * smallest value <i>k</i> such that:
	  * <blockquote><pre>
	  * (this.codePointAt(<i>k</i>) == ch) {@code &&} (<i>k</i> &gt;= fromIndex)
	  * </pre></blockquote>
	  * is true. In either case, if no such character occurs in this
	  * string at or after position {@code fromIndex}, then
	  * {@code -1} is returned.
	  *
	  * <p>
	  * There is no restriction on the value of {@code fromIndex}. If it
	  * is negative, it has the same effect as if it were zero: this entire
	  * string may be searched. If it is greater than the length of this
	  * string, it has the same effect as if it were equal to the length of
	  * this string: {@code -1} is returned.
	  *
	  * <p>All indices are specified in {@code char} values
	  * (Unicode code units).
	  *
	  * @param   ch        a character (Unicode code point).
	  * @param   fromIndex the index to start the search from.
	  * @return the index of the first occurrence of the character in the
	  *         character sequence represented by this object that is greater
	  *         than or equal to { @code fromIndex}, or { @code -1}
	  *                                  if the character does not occur.
	  */
	def indexOf(ch: Int, fromIndex: Int): Int = {
		val max: Int = value.length
		if (fromIndex < 0) {
			fromIndex = 0
		}
		else if (fromIndex >= max) {
			// Note: fromIndex might be near -1>>>1.
			return -1
		}
		if (ch < Character.MIN_SUPPLEMENTARY_CODE_POINT) {
			// handle most cases here (ch is a BMP code point or a
			// negative value (invalid code point))
			val value: Array[Char] = this.value
			var i: Int = fromIndex
			while (i < max) {
				{
					if (value(i) == ch) {
						return i
					}
				}
				{i += 1; i - 1 }
			}
			return -1
		}
		else
		{
			return indexOfSupplementary(ch, fromIndex)
		}
	}
	
	/**
	  * Handles (rare) calls of indexOf with a supplementary character.
	  */
	private def indexOfSupplementary(ch: Int, fromIndex: Int): Int = {
		if (Character.isValidCodePoint(ch)) {
			val value: Array[Char] = this.value
			val hi: Char = Character.highSurrogate(ch)
			val lo: Char = Character.lowSurrogate(ch)
			val max: Int = value.length - 1
			var i: Int = fromIndex
			while (i < max) {
				{
					if (value(i) == hi && value(i + 1) == lo) {
						return i
					}
				}
				{i += 1; i - 1 }
			}
		}
		return -1
	}
	
	/**
	  * Returns the index within this string of the last occurrence of
	  * the specified character. For values of {@code ch} in the
	  * range from 0 to 0xFFFF (inclusive), the index (in Unicode code
	  * units) returned is the largest value <i>k</i> such that:
	  * <blockquote><pre>
	  * this.charAt(<i>k</i>) == ch
	  * </pre></blockquote>
	  * is true. For other values of {@code ch}, it is the
	  * largest value <i>k</i> such that:
	  * <blockquote><pre>
	  * this.codePointAt(<i>k</i>) == ch
	  * </pre></blockquote>
	  * is true.  In either case, if no such character occurs in this
	  * string, then {@code -1} is returned.  The
	  * {@code String} is searched backwards starting at the last
	  * character.
	  *
	  * @param   ch a character (Unicode code point).
	  * @return the index of the last occurrence of the character in the
	  *         character sequence represented by this object, or
	  *         { @code -1} if the character does not occur.
	  */
	def lastIndexOf(ch: Int): Int = {
		return lastIndexOf(ch, value.length - 1)
	}
	
	/**
	  * Returns the index within this string of the last occurrence of
	  * the specified character, searching backward starting at the
	  * specified index. For values of {@code ch} in the range
	  * from 0 to 0xFFFF (inclusive), the index returned is the largest
	  * value <i>k</i> such that:
	  * <blockquote><pre>
	  * (this.charAt(<i>k</i>) == ch) {@code &&} (<i>k</i> &lt;= fromIndex)
	  * </pre></blockquote>
	  * is true. For other values of {@code ch}, it is the
	  * largest value <i>k</i> such that:
	  * <blockquote><pre>
	  * (this.codePointAt(<i>k</i>) == ch) {@code &&} (<i>k</i> &lt;= fromIndex)
	  * </pre></blockquote>
	  * is true. In either case, if no such character occurs in this
	  * string at or before position {@code fromIndex}, then
	  * {@code -1} is returned.
	  *
	  * <p>All indices are specified in {@code char} values
	  * (Unicode code units).
	  *
	  * @param   ch        a character (Unicode code point).
	  * @param   fromIndex the index to start the search from. There is no
	  *                    restriction on the value of { @code fromIndex}. If it is
	  *                                                        greater than or equal to the length of this string, it has
	  *                                                        the same effect as if it were equal to one less than the
	  *                                                        length of this string: this entire string may be searched.
	  *                                                        If it is negative, it has the same effect as if it were -1:
	  *                                                        -1 is returned.
	  * @return the index of the last occurrence of the character in the
	  *         character sequence represented by this object that is less
	  *         than or equal to { @code fromIndex}, or { @code -1}
	  *                                  if the character does not occur before that point.
	  */
	def lastIndexOf(ch: Int, fromIndex: Int): Int = {
		if (ch < Character.MIN_SUPPLEMENTARY_CODE_POINT) {
			// handle most cases here (ch is a BMP code point or a
			// negative value (invalid code point))
			val value: Array[Char] = this.value
			var i: Int = Math.min(fromIndex, value.length - 1)
			while (i >= 0) {
				{
					if (value(i) == ch) {
						return i
					}
				}
				{i -= 1; i + 1 }
			}
			return -1
		}
		else
		{
			return lastIndexOfSupplementary(ch, fromIndex)
		}
	}
	
	/**
	  * Handles (rare) calls of lastIndexOf with a supplementary character.
	  */
	private def lastIndexOfSupplementary(ch: Int, fromIndex: Int): Int = {
		if (Character.isValidCodePoint(ch)) {
			val value: Array[Char] = this.value
			val hi: Char = Character.highSurrogate(ch)
			val lo: Char = Character.lowSurrogate(ch)
			var i: Int = Math.min(fromIndex, value.length - 2)
			while (i >= 0) {
				{
					if (value(i) == hi && value(i + 1) == lo) {
						return i
					}
				}
				{i -= 1; i + 1 }
			}
		}
		return -1
	}
	
	/**
	  * Returns the index within this string of the first occurrence of the
	  * specified substring.
	  *
	  * <p>The returned index is the smallest value <i>k</i> for which:
	  * <blockquote><pre>
	  * this.startsWith(str, <i>k</i>)
	  * </pre></blockquote>
	  * If no such value of <i>k</i> exists, then {@code -1} is returned.
	  *
	  * @param   str the substring to search for.
	  * @return the index of the first occurrence of the specified substring,
	  *         or { @code -1} if there is no such occurrence.
	  */
	def indexOf(str: String): Int = {
		return indexOf(str, 0)
	}
	
	/**
	  * Returns the index within this string of the first occurrence of the
	  * specified substring, starting at the specified index.
	  *
	  * <p>The returned index is the smallest value <i>k</i> for which:
	  * <blockquote><pre>
	  * <i>k</i> &gt;= fromIndex {@code &&} this.startsWith(str, <i>k</i>)
	  * </pre></blockquote>
	  * If no such value of <i>k</i> exists, then {@code -1} is returned.
	  *
	  * @param   str       the substring to search for.
	  * @param   fromIndex the index from which to start the search.
	  * @return the index of the first occurrence of the specified substring,
	  *         starting at the specified index,
	  *         or { @code -1} if there is no such occurrence.
	  */
	def indexOf(str: String, fromIndex: Int): Int = {
		return indexOf(value, 0, value.length, str.value, 0, str.value.length, fromIndex)
	}
	
	/**
	  * Code shared by String and AbstractStringBuilder to do searches. The
	  * source is the character array being searched, and the target
	  * is the string being searched for.
	  *
	  * @param   source       the characters being searched.
	  * @param   sourceOffset offset of the source string.
	  * @param   sourceCount  count of the source string.
	  * @param   target       the characters being searched for.
	  * @param   fromIndex    the index to begin searching from.
	  */
	private[lang] def indexOf(source: Array[Char], sourceOffset: Int, sourceCount: Int, target: String, fromIndex: Int
	                         ): Int =
	{
		return indexOf(source, sourceOffset, sourceCount, target.value, 0, target.value.length, fromIndex)
	}
	
	/**
	  * Code shared by String and StringBuffer to do searches. The
	  * source is the character array being searched, and the target
	  * is the string being searched for.
	  *
	  * @param   source       the characters being searched.
	  * @param   sourceOffset offset of the source string.
	  * @param   sourceCount  count of the source string.
	  * @param   target       the characters being searched for.
	  * @param   targetOffset offset of the target string.
	  * @param   targetCount  count of the target string.
	  * @param   fromIndex    the index to begin searching from.
	  */
	private[lang] def indexOf(source: Array[Char], sourceOffset: Int, sourceCount: Int, target: Array[Char],
	                          targetOffset: Int, targetCount: Int, fromIndex: Int
	                         ): Int =
	{
		if (fromIndex >= sourceCount) {
			return (if (targetCount == 0) sourceCount
			        else -1)
		}
		if (fromIndex < 0) {
			fromIndex = 0
		}
		if (targetCount == 0) {
			return fromIndex
		}
		val first: Char = target(targetOffset)
		val max: Int = sourceOffset + (sourceCount - targetCount)
		var i: Int = sourceOffset + fromIndex
		while (i <= max) {
			{
				/* Look for first character. */ if (source(i) != first) {
				while ( { i += 1; i } <= max && source(i) != first) {}
			}
				/* Found first character, now look at the rest of v2 */ if (i <= max) {
				var j: Int = i + 1
				val end: Int = j + targetCount - 1
				var k: Int = targetOffset + 1
				while (j < end && source(j) == target(k)) {
					{j += 1; j - 1 }
					{k += 1; k - 1 }
				}
				if (j == end) {
					/* Found whole string. */ return i - sourceOffset
				}
			}
			}
			{i += 1; i - 1 }
		}
		return -1
	}
	
	/**
	  * Returns the index within this string of the last occurrence of the
	  * specified substring.  The last occurrence of the empty string ""
	  * is considered to occur at the index value {@code this.length()}.
	  *
	  * <p>The returned index is the largest value <i>k</i> for which:
	  * <blockquote><pre>
	  * this.startsWith(str, <i>k</i>)
	  * </pre></blockquote>
	  * If no such value of <i>k</i> exists, then {@code -1} is returned.
	  *
	  * @param   str the substring to search for.
	  * @return the index of the last occurrence of the specified substring,
	  *         or { @code -1} if there is no such occurrence.
	  */
	def lastIndexOf(str: String): Int = {
		return lastIndexOf(str, value.length)
	}
	
	/**
	  * Returns the index within this string of the last occurrence of the
	  * specified substring, searching backward starting at the specified index.
	  *
	  * <p>The returned index is the largest value <i>k</i> for which:
	  * <blockquote><pre>
	  * <i>k</i> {@code <=} fromIndex {@code &&} this.startsWith(str, <i>k</i>)
	  * </pre></blockquote>
	  * If no such value of <i>k</i> exists, then {@code -1} is returned.
	  *
	  * @param   str       the substring to search for.
	  * @param   fromIndex the index to start the search from.
	  * @return the index of the last occurrence of the specified substring,
	  *         searching backward from the specified index,
	  *         or { @code -1} if there is no such occurrence.
	  */
	def lastIndexOf(str: String, fromIndex: Int): Int = {
		return lastIndexOf(value, 0, value.length, str.value, 0, str.value.length, fromIndex)
	}
	
	/**
	  * Code shared by String and AbstractStringBuilder to do searches. The
	  * source is the character array being searched, and the target
	  * is the string being searched for.
	  *
	  * @param   source       the characters being searched.
	  * @param   sourceOffset offset of the source string.
	  * @param   sourceCount  count of the source string.
	  * @param   target       the characters being searched for.
	  * @param   fromIndex    the index to begin searching from.
	  */
	private[lang] def lastIndexOf(source: Array[Char], sourceOffset: Int, sourceCount: Int, target: String,
	                              fromIndex: Int
	                             ): Int =
	{
		return lastIndexOf(source, sourceOffset, sourceCount, target.value, 0, target.value.length, fromIndex)
	}
	
	/**
	  * Code shared by String and StringBuffer to do searches. The
	  * source is the character array being searched, and the target
	  * is the string being searched for.
	  *
	  * @param   source       the characters being searched.
	  * @param   sourceOffset offset of the source string.
	  * @param   sourceCount  count of the source string.
	  * @param   target       the characters being searched for.
	  * @param   targetOffset offset of the target string.
	  * @param   targetCount  count of the target string.
	  * @param   fromIndex    the index to begin searching from.
	  */
	private[lang] def lastIndexOf(source: Array[Char], sourceOffset: Int, sourceCount: Int, target: Array[Char],
	                              targetOffset: Int, targetCount: Int, fromIndex: Int
	                             ): Int =
	{
		/*
         * Check arguments; return immediately where possible. For
         * consistency, don't check for null str.
         */ val rightIndex: Int = sourceCount - targetCount
		if (fromIndex < 0) {
			return -1
		}
		if (fromIndex > rightIndex) {
			fromIndex = rightIndex
		}
		/* Empty string always matches. */ if (targetCount == 0) {
		return fromIndex
	}
		val strLastIndex: Int = targetOffset + targetCount - 1
		val strLastChar: Char = target(strLastIndex)
		val min: Int = sourceOffset + targetCount - 1
		var i: Int = min + fromIndex
		startSearchForLastChar //todo: labels is not supported
		while (true) {
			{
				while (i >= min && source(i) != strLastChar) {
					{
						i -= 1
					}
				}
				if (i < min) {
					return -1
				}
				var j: Int = i - 1
				val start: Int = j - (targetCount - 1)
				var k: Int = strLastIndex - 1
				while (j > start) {
					{
						if (source({ j -= 1; j + 1 }) != target({ k -= 1; k + 1 })) {
							i -= 1
							continue startSearchForLastChar //todo: continue is not supported
						}
					}
				}
				return start - sourceOffset + 1
			}
		}
	}
	
	/**
	  * Returns a string that is a substring of this string. The
	  * substring begins with the character at the specified index and
	  * extends to the end of this string. <p>
	  * Examples:
	  * <blockquote><pre>
	  * "unhappy".substring(2) returns "happy"
	  * "Harbison".substring(3) returns "bison"
	  * "emptiness".substring(9) returns "" (an empty string)
	  * </pre></blockquote>
	  *
	  * @param      beginIndex the beginning index, inclusive.
	  * @return the specified substring.
	  * @exception IndexOutOfBoundsException  if
	  *            { @code beginIndex} is negative or larger than the
	  *                    length of this { @code String} object.
	  */
	def substring(beginIndex: Int): String = {
		if (beginIndex < 0) {
			throw new StringIndexOutOfBoundsException(beginIndex)
		}
		val subLen: Int = value.length - beginIndex
		if (subLen < 0) {
			throw new StringIndexOutOfBoundsException(subLen)
		}
		return if ((beginIndex == 0)) this
		       else new String(value, beginIndex, subLen)
	}
	
	/**
	  * Returns a string that is a substring of this string. The
	  * substring begins at the specified {@code beginIndex} and
	  * extends to the character at index {@code endIndex - 1}.
	  * Thus the length of the substring is {@code endIndex-beginIndex}.
	  * <p>
	  * Examples:
	  * <blockquote><pre>
	  * "hamburger".substring(4, 8) returns "urge"
	  * "smiles".substring(1, 5) returns "mile"
	  * </pre></blockquote>
	  *
	  * @param      beginIndex the beginning index, inclusive.
	  * @param      endIndex   the ending index, exclusive.
	  * @return the specified substring.
	  * @exception IndexOutOfBoundsException  if the
	  *            { @code beginIndex} is negative, or
	  *                    { @code endIndex} is larger than the length of
	  *                    this { @code String} object, or
	  *                    { @code beginIndex} is larger than
	  *                    { @code endIndex}.
	  */
	def substring(beginIndex: Int, endIndex: Int): String = {
		if (beginIndex < 0) {
			throw new StringIndexOutOfBoundsException(beginIndex)
		}
		if (endIndex > value.length) {
			throw new StringIndexOutOfBoundsException(endIndex)
		}
		val subLen: Int = endIndex - beginIndex
		if (subLen < 0) {
			throw new StringIndexOutOfBoundsException(subLen)
		}
		return if (((beginIndex == 0) && (endIndex == value.length))) this
		       else new String(value, beginIndex, subLen)
	}
	
	/**
	  * Returns a character sequence that is a subsequence of this sequence.
	  *
	  * <p> An invocation of this method of the form
	  *
	  * <blockquote><pre>
	  * str.subSequence(begin,&nbsp;end)</pre></blockquote>
	  *
	  * behaves in exactly the same way as the invocation
	  *
	  * <blockquote><pre>
	  * str.substring(begin,&nbsp;end)</pre></blockquote>
	  *
	  * @apiNote
	  * This method is defined so that the { @code String} class can implement
	  *                                            the { @link CharSequence} interface.
	  *
	  * @param   beginIndex the begin index, inclusive.
	  * @param   endIndex   the end index, exclusive.
	  * @return the specified subsequence.
	  *
	  * @throws  IndexOutOfBoundsException
	  * if { @code beginIndex} or { @code endIndex} is negative,
	  *            if { @code endIndex} is greater than { @code length()},
	  *            or if { @code beginIndex} is greater than { @code endIndex}
	  *
	  * @since 1.4
	  * @spec JSR-51
	  */
	def subSequence(beginIndex: Int, endIndex: Int): CharSequence = {
		return this.substring(beginIndex, endIndex)
	}
	
	/**
	  * Concatenates the specified string to the end of this string.
	  * <p>
	  * If the length of the argument string is {@code 0}, then this
	  * {@code String} object is returned. Otherwise, a
	  * {@code String} object is returned that represents a character
	  * sequence that is the concatenation of the character sequence
	  * represented by this {@code String} object and the character
	  * sequence represented by the argument string.<p>
	  * Examples:
	  * <blockquote><pre>
	  * "cares".concat("s") returns "caress"
	  * "to".concat("get").concat("her") returns "together"
	  * </pre></blockquote>
	  *
	  * @param   str the { @code String} that is concatenated to the end
	  *                          of this { @code String}.
	  * @return a string that represents the concatenation of this object's
	  *         characters followed by the string argument's characters.
	  */
	def concat(str: String): String = {
		val otherLen: Int = str.length
		if (otherLen == 0) {
			return this
		}
		val len: Int = value.length
		val buf: Array[Char] = Arrays.copyOf(value, len + otherLen)
		str.getChars(buf, len)
		return new String(buf, true)
	}
	
	/**
	  * Returns a string resulting from replacing all occurrences of
	  * {@code oldChar} in this string with {@code newChar}.
	  * <p>
	  * If the character {@code oldChar} does not occur in the
	  * character sequence represented by this {@code String} object,
	  * then a reference to this {@code String} object is returned.
	  * Otherwise, a {@code String} object is returned that
	  * represents a character sequence identical to the character sequence
	  * represented by this {@code String} object, except that every
	  * occurrence of {@code oldChar} is replaced by an occurrence
	  * of {@code newChar}.
	  * <p>
	  * Examples:
	  * <blockquote><pre>
	  * "mesquite in your cellar".replace('e', 'o')
	  * returns "mosquito in your collar"
	  * "the war of baronets".replace('r', 'y')
	  * returns "the way of bayonets"
	  * "sparring with a purple porpoise".replace('p', 't')
	  * returns "starring with a turtle tortoise"
	  * "JonL".replace('q', 'x') returns "JonL" (no change)
	  * </pre></blockquote>
	  *
	  * @param   oldChar the old character.
	  * @param   newChar the new character.
	  * @return a string derived from this string by replacing every
	  *         occurrence of { @code oldChar} with { @code newChar}.
	  */
	def replace(oldChar: Char, newChar: Char): String = {
		if (oldChar != newChar) {
			val len: Int = value.length
			var i: Int = -1
			val `val`: Array[Char] = value /* avoid getfield opcode */
			while ( { i += 1; i } < len) {
				{
					if (`val`(i) == oldChar) {
						break //todo: break is not supported
					}
				}
			}
			if (i < len) {
				val buf: Array[Char] = new Array[Char](len)
				var j: Int = 0
				while (j < i) {
					{
						buf(j) = `val`(j)
					}
					{j += 1; j - 1 }
				}
				while (i < len) {
					{
						val c: Char = `val`(i)
						buf(i) = if ((c == oldChar)) newChar
						         else c
						i += 1
					}
				}
				return new String(buf, true)
			}
		}
		return this
	}
	
	/**
	  * Tells whether or not this string matches the given <a
	  * href="../util/regex/Pattern.html#sum">regular expression</a>.
	  *
	  * <p> An invocation of this method of the form
	  * <i>str</i>{@code .matches(}<i>regex</i>{@code )} yields exactly the
	  * same result as the expression
	  *
	  * <blockquote>
	  * {@link java.util.regex.Pattern}.{@link java.util.regex.Pattern#matches(String,CharSequence)
     * matches(<i>regex</i>, <i>str</i>)}
	  * </blockquote>
	  *
	  * @param   regex
	  * the regular expression to which this string is to be matched
	  *
	  * @return { @code true} if, and only if, this string matches the
	  *                 given regular expression
	  *
	  * @throws  PatternSyntaxException
	  * if the regular expression's syntax is invalid
	  *
	  * @see java.util.regex.Pattern
	  *
	  * @since 1.4
	  * @spec JSR-51
	  */
	def matches(regex: String): Boolean = {
		return Pattern.matches(regex, this)
	}
	
	/**
	  * Returns true if and only if this string contains the specified
	  * sequence of char values.
	  *
	  * @param s the sequence to search for
	  * @return true if this string contains { @code s}, false otherwise
	  * @since 1.5
	  */
	def contains(s: CharSequence): Boolean = {
		return indexOf(s.toString) > -1
	}
	
	/**
	  * Replaces the first substring of this string that matches the given <a
	  * href="../util/regex/Pattern.html#sum">regular expression</a> with the
	  * given replacement.
	  *
	  * <p> An invocation of this method of the form
	  * <i>str</i>{@code .replaceFirst(}<i>regex</i>{@code ,} <i>repl</i>{@code )}
	  * yields exactly the same result as the expression
	  *
	  * <blockquote>
	  * <code>
	  * {@link java.util.regex.Pattern}.{@link
	  * java.util.regex.Pattern#compile compile}(<i>regex</i>).{@link
	  * java.util.regex.Pattern#matcher(java.lang.CharSequence) matcher}(<i>str</i>).{@link
	  * java.util.regex.Matcher#replaceFirst replaceFirst}(<i>repl</i>)
	  * </code>
	  * </blockquote>
	  *
	  * <p>
	  * Note that backslashes ({@code \}) and dollar signs ({@code $}) in the
	  * replacement string may cause the results to be different than if it were
	  * being treated as a literal replacement string; see
	  * {@link java.util.regex.Matcher#replaceFirst}.
	  * Use {@link java.util.regex.Matcher#quoteReplacement} to suppress the special
	  * meaning of these characters, if desired.
	  *
	  * @param   regex
	  * the regular expression to which this string is to be matched
	  * @param   replacement
	  * the string to be substituted for the first match
	  *
	  * @return The resulting { @code String}
	  *
	  * @throws  PatternSyntaxException
	  * if the regular expression's syntax is invalid
	  *
	  * @see java.util.regex.Pattern
	  *
	  * @since 1.4
	  * @spec JSR-51
	  */
	def replaceFirst(regex: String, replacement: String): String = {
		return Pattern.compile(regex).matcher(this).replaceFirst(replacement)
	}
	
	/**
	  * Replaces each substring of this string that matches the given <a
	  * href="../util/regex/Pattern.html#sum">regular expression</a> with the
	  * given replacement.
	  *
	  * <p> An invocation of this method of the form
	  * <i>str</i>{@code .replaceAll(}<i>regex</i>{@code ,} <i>repl</i>{@code )}
	  * yields exactly the same result as the expression
	  *
	  * <blockquote>
	  * <code>
	  * {@link java.util.regex.Pattern}.{@link
	  * java.util.regex.Pattern#compile compile}(<i>regex</i>).{@link
	  * java.util.regex.Pattern#matcher(java.lang.CharSequence) matcher}(<i>str</i>).{@link
	  * java.util.regex.Matcher#replaceAll replaceAll}(<i>repl</i>)
	  * </code>
	  * </blockquote>
	  *
	  * <p>
	  * Note that backslashes ({@code \}) and dollar signs ({@code $}) in the
	  * replacement string may cause the results to be different than if it were
	  * being treated as a literal replacement string; see
	  * {@link java.util.regex.Matcher#replaceAll Matcher.replaceAll}.
	  * Use {@link java.util.regex.Matcher#quoteReplacement} to suppress the special
	  * meaning of these characters, if desired.
	  *
	  * @param   regex
	  * the regular expression to which this string is to be matched
	  * @param   replacement
	  * the string to be substituted for each match
	  *
	  * @return The resulting { @code String}
	  *
	  * @throws  PatternSyntaxException
	  * if the regular expression's syntax is invalid
	  *
	  * @see java.util.regex.Pattern
	  *
	  * @since 1.4
	  * @spec JSR-51
	  */
	def replaceAll(regex: String, replacement: String): String = {
		return Pattern.compile(regex).matcher(this).replaceAll(replacement)
	}
	
	/**
	  * Replaces each substring of this string that matches the literal target
	  * sequence with the specified literal replacement sequence. The
	  * replacement proceeds from the beginning of the string to the end, for
	  * example, replacing "aa" with "b" in the string "aaa" will result in
	  * "ba" rather than "ab".
	  *
	  * @param  target      The sequence of char values to be replaced
	  * @param  replacement The replacement sequence of char values
	  * @return The resulting string
	  * @since 1.5
	  */
	def replace(target: CharSequence, replacement: CharSequence): String = {
		return Pattern.compile(target.toString, Pattern.LITERAL).matcher(this)
			.replaceAll(Matcher.quoteReplacement(replacement.toString))
	}
	
	/**
	  * Splits this string around matches of the given
	  * <a href="../util/regex/Pattern.html#sum">regular expression</a>.
	  *
	  * <p> The array returned by this method contains each substring of this
	  * string that is terminated by another substring that matches the given
	  * expression or is terminated by the end of the string.  The substrings in
	  * the array are in the order in which they occur in this string.  If the
	  * expression does not match any part of the input then the resulting array
	  * has just one element, namely this string.
	  *
	  * <p> When there is a positive-width match at the beginning of this
	  * string then an empty leading substring is included at the beginning
	  * of the resulting array. A zero-width match at the beginning however
	  * never produces such empty leading substring.
	  *
	  * <p> The {@code limit} parameter controls the number of times the
	  * pattern is applied and therefore affects the length of the resulting
	  * array.  If the limit <i>n</i> is greater than zero then the pattern
	  * will be applied at most <i>n</i>&nbsp;-&nbsp;1 times, the array's
	  * length will be no greater than <i>n</i>, and the array's last entry
	  * will contain all input beyond the last matched delimiter.  If <i>n</i>
	  * is non-positive then the pattern will be applied as many times as
	  * possible and the array can have any length.  If <i>n</i> is zero then
	  * the pattern will be applied as many times as possible, the array can
	  * have any length, and trailing empty strings will be discarded.
	  *
	  * <p> The string {@code "boo:and:foo"}, for example, yields the
	  * following results with these parameters:
	  *
	  * <blockquote><table cellpadding=1 cellspacing=0 summary="Split example showing regex, limit, and result">
	  * <tr>
	  * <th>Regex</th>
	  * <th>Limit</th>
	  * <th>Result</th>
	  * </tr>
	  * <tr><td align=center>:</td>
	  * <td align=center>2</td>
	  * <td>{@code { "boo", "and:foo" }}</td></tr>
	  * <tr><td align=center>:</td>
	  * <td align=center>5</td>
	  * <td>{@code { "boo", "and", "foo" }}</td></tr>
	  * <tr><td align=center>:</td>
	  * <td align=center>-2</td>
	  * <td>{@code { "boo", "and", "foo" }}</td></tr>
	  * <tr><td align=center>o</td>
	  * <td align=center>5</td>
	  * <td>{@code { "b", "", ":and:f", "", "" }}</td></tr>
	  * <tr><td align=center>o</td>
	  * <td align=center>-2</td>
	  * <td>{@code { "b", "", ":and:f", "", "" }}</td></tr>
	  * <tr><td align=center>o</td>
	  * <td align=center>0</td>
	  * <td>{@code { "b", "", ":and:f" }}</td></tr>
	  * </table></blockquote>
	  *
	  * <p> An invocation of this method of the form
	  * <i>str.</i>{@code split(}<i>regex</i>{@code ,}&nbsp;<i>n</i>{@code )}
	  * yields the same result as the expression
	  *
	  * <blockquote>
	  * <code>
	  * {@link java.util.regex.Pattern}.{@link
	  * java.util.regex.Pattern#compile compile}(<i>regex</i>).{@link
	  * java.util.regex.Pattern#split(java.lang.CharSequence,int) split}(<i>str</i>,&nbsp;<i>n</i>)
	  * </code>
	  * </blockquote>
	  *
	  *
	  * @param  regex
	  * the delimiting regular expression
	  *
	  * @param  limit
	  * the result threshold, as described above
	  *
	  * @return the array of strings computed by splitting this string
	  *         around matches of the given regular expression
	  *
	  * @throws  PatternSyntaxException
	  * if the regular expression's syntax is invalid
	  *
	  * @see java.util.regex.Pattern
	  *
	  * @since 1.4
	  * @spec JSR-51
	  */
	def split(regex: String, limit: Int): Array[String] = {
		/* fastpath if the regex is a
         (1)one-char String and this character is not one of the
            RegEx's meta characters ".$|()[{^?*+\\", or
         (2)two-char String and the first char is the backslash and
            the second is not the ascii digit or ascii letter.
         */ var ch: Char = 0
		if (((regex.value.length == 1 && ".$|()[{^?*+\\".indexOf(ch = regex.charAt(0)) == -1) || (regex
			.length == 2 && regex.charAt(0) == '\\' && (((ch = regex.charAt(
			1)) - '0') | ('9' - ch)) < 0 && ((ch - 'a') | ('z' - ch)) < 0 && ((ch - 'A') | ('Z' - ch)) < 0)) && (ch < Character
			.MIN_HIGH_SURROGATE || ch > Character.MAX_LOW_SURROGATE))
		{
			var off: Int = 0
			var next: Int = 0
			val limited: Boolean = limit > 0
			val list: util.ArrayList[String] = new util.ArrayList[String]
			while ((next = indexOf(ch, off)) != -1) {
				{
					if (!limited || list.size < limit - 1) {
						list.add(substring(off, next))
						off = next + 1
					}
					else
					{
						// last one
						//assert (list.size() == limit - 1);
						list.add(substring(off, value.length))
						off = value.length
						break //todo: break is not supported
					}
				}
			}
			// If no match was found, return this
			if (off == 0) return Array[String](this)
			// Add remaining segment
			if (!limited || list.size < limit) list.add(substring(off, value.length))
			// Construct result
			var resultSize: Int = list.size
			if (limit == 0) {
				while (resultSize > 0 && list.get(resultSize - 1).length == 0) {
					{
						resultSize -= 1
					}
				}
			}
			val result: Array[String] = new Array[String](resultSize)
			return list.subList(0, resultSize).toArray(result)
		}
		return Pattern.compile(regex).split(this, limit)
	}
	
	/**
	  * Splits this string around matches of the given <a
	  * href="../util/regex/Pattern.html#sum">regular expression</a>.
	  *
	  * <p> This method works as if by invoking the two-argument {@link
	  * #split(String, int) split} method with the given expression and a limit
	  * argument of zero.  Trailing empty strings are therefore not included in
	  * the resulting array.
	  *
	  * <p> The string {@code "boo:and:foo"}, for example, yields the following
	  * results with these expressions:
	  *
	  * <blockquote><table cellpadding=1 cellspacing=0 summary="Split examples showing regex and result">
	  * <tr>
	  * <th>Regex</th>
	  * <th>Result</th>
	  * </tr>
	  * <tr><td align=center>:</td>
	  * <td>{@code { "boo", "and", "foo" }}</td></tr>
	  * <tr><td align=center>o</td>
	  * <td>{@code { "b", "", ":and:f" }}</td></tr>
	  * </table></blockquote>
	  *
	  *
	  * @param  regex
	  * the delimiting regular expression
	  *
	  * @return the array of strings computed by splitting this string
	  *         around matches of the given regular expression
	  *
	  * @throws  PatternSyntaxException
	  * if the regular expression's syntax is invalid
	  *
	  * @see java.util.regex.Pattern
	  *
	  * @since 1.4
	  * @spec JSR-51
	  */
	def split(regex: String): Array[String] = {
		return split(regex, 0)
	}
	
	/**
	  * Returns a new String composed of copies of the
	  * {@code CharSequence elements} joined together with a copy of
	  * the specified {@code delimiter}.
	  *
	  * <blockquote>For example,
	  * <pre>{@code
	  * String message = String.join("-", "Java", "is", "cool");
     *     // message returned is: "Java-is-cool"
     * }</pre></blockquote>
	  *
	  * Note that if an element is null, then {@code "null"} is added.
	  *
	  * @param  delimiter the delimiter that separates each element
	  * @param  elements  the elements to join together.
	  *
	  * @return a new { @code String} that is composed of the { @code elements}
	  *                       separated by the { @code delimiter}
	  *
	  * @throws NullPointerException If { @code delimiter} or { @code elements}
	  *                                         is { @code null}
	  *
	  * @see java.util.StringJoiner
	  * @since 1.8
	  */
	def join(delimiter: CharSequence, elements: CharSequence*): String = {
		Objects.requireNonNull(delimiter)
		Objects.requireNonNull(elements)
		// Number of elements not likely worth Arrays.stream overhead.
		val joiner: StringJoiner = new StringJoiner(delimiter)
		for (cs <- elements) {
			joiner.add(cs)
		}
		return joiner.toString
	}
	
	/**
	  * Returns a new {@code String} composed of copies of the
	  * {@code CharSequence elements} joined together with a copy of the
	  * specified {@code delimiter}.
	  *
	  * <blockquote>For example,
	  * <pre>{@code
	  * List<String> strings = new LinkedList<>();
     *     strings.add("Java");strings.add("is");
     *     strings.add("cool");
     *     String message = String.join(" ", strings);
     *     //message returned is: "Java is cool"
     *
     *     Set<String> strings = new LinkedHashSet<>();
     *     strings.add("Java"); strings.add("is");
     *     strings.add("very"); strings.add("cool");
     *     String message = String.join("-", strings);
     *     //message returned is: "Java-is-very-cool"
     * }</pre></blockquote>
	  *
	  * Note that if an individual element is {@code null}, then {@code "null"} is added.
	  *
	  * @param  delimiter a sequence of characters that is used to separate each
	  *                   of the { @code elements} in the resulting { @code String}
	  * @param  elements an { @code Iterable} that will have its { @code elements}
	  *                             joined together.
	  *
	  * @return a new { @code String} that is composed from the { @code elements}
	  *                       argument
	  *
	  * @throws NullPointerException If { @code delimiter} or { @code elements}
	  *                                         is { @code null}
	  *
	  * @see #join(CharSequence,CharSequence...)
	  * @see java.util.StringJoiner
	  * @since 1.8
	  */
	def join(delimiter: CharSequence, elements: Iterable[_ <: CharSequence]): String = {
		Objects.requireNonNull(delimiter)
		Objects.requireNonNull(elements)
		val joiner: StringJoiner = new StringJoiner(delimiter)
		import scala.collection.JavaConversions._
		for (cs <- elements) {
			joiner.add(cs)
		}
		return joiner.toString
	}
	
	/**
	  * Converts all of the characters in this {@code String} to lower
	  * case using the rules of the given {@code Locale}.  Case mapping is based
	  * on the Unicode Standard version specified by the {@link java.lang.Character Character}
	  * class. Since case mappings are not always 1:1 char mappings, the resulting
	  * {@code String} may be a different length than the original {@code String}.
	  * <p>
	  * Examples of lowercase  mappings are in the following table:
	  * <table border="1" summary="Lowercase mapping examples showing language code of locale, upper case, lower case, and description">
	  * <tr>
	  * <th>Language Code of Locale</th>
	  * <th>Upper Case</th>
	  * <th>Lower Case</th>
	  * <th>Description</th>
	  * </tr>
	  * <tr>
	  * <td>tr (Turkish)</td>
	  * <td>&#92;u0130</td>
	  * <td>&#92;u0069</td>
	  * <td>capital letter I with dot above -&gt; small letter i</td>
	  * </tr>
	  * <tr>
	  * <td>tr (Turkish)</td>
	  * <td>&#92;u0049</td>
	  * <td>&#92;u0131</td>
	  * <td>capital letter I -&gt; small letter dotless i </td>
	  * </tr>
	  * <tr>
	  * <td>(all)</td>
	  * <td>French Fries</td>
	  * <td>french fries</td>
	  * <td>lowercased all chars in String</td>
	  * </tr>
	  * <tr>
	  * <td>(all)</td>
	  * <td><img src="doc-files/capiota.gif" alt="capiota"><img src="doc-files/capchi.gif" alt="capchi">
	  * <img src="doc-files/captheta.gif" alt="captheta"><img src="doc-files/capupsil.gif" alt="capupsil">
	  * <img src="doc-files/capsigma.gif" alt="capsigma"></td>
	  * <td><img src="doc-files/iota.gif" alt="iota"><img src="doc-files/chi.gif" alt="chi">
	  * <img src="doc-files/theta.gif" alt="theta"><img src="doc-files/upsilon.gif" alt="upsilon">
	  * <img src="doc-files/sigma1.gif" alt="sigma"></td>
	  * <td>lowercased all chars in String</td>
	  * </tr>
	  * </table>
	  *
	  * @param locale use the case transformation rules for this locale
	  * @return the { @code String}, converted to lowercase.
	  * @see java.lang.String#toLowerCase()
	  * @see java.lang.String#toUpperCase()
	  * @see java.lang.String#toUpperCase(Locale)
	  * @since 1.1
	  */
	def toLowerCase(locale: Locale): String = {
		if (locale == null) {
			throw new NullPointerException
		}
		var firstUpper: Int = 0
		val len: Int = value.length
		/* Now check if there are any characters that need to be changed. */ scan //todo: labels is not supported
		{
			firstUpper = 0
			while (firstUpper < len) {
				{
					val c: Char = value(firstUpper)
					if ((c >= Character.MIN_HIGH_SURROGATE) && (c <= Character.MAX_HIGH_SURROGATE)) {
						val supplChar: Int = codePointAt(firstUpper)
						if (supplChar != Character.toLowerCase(supplChar)) {
							break scan // todo: label break is not supported
						}
						firstUpper += Character.charCount(supplChar)
					}
					else
					{
						if (c != Character.toLowerCase(c)) {
							break scan // todo: label break is not supported
						}
						firstUpper += 1
					}
				}
			}
			return this
		}
		var result: Array[Char] = new Array[Char](len)
		var resultOffset: Int = 0
		/* result may grow, so i+resultOffset
                                * is the write location in result */
		/* Just copy the first few lowerCase characters. */ System.arraycopy(value, 0, result, 0, firstUpper)
		val lang: String = locale.getLanguage
		val localeDependent: Boolean = ((lang eq "tr") || (lang eq "az") || (lang eq "lt"))
		var lowerCharArray: Array[Char] = null
		var lowerChar: Int = 0
		var srcChar: Int = 0
		var srcCount: Int = 0
		var i: Int = firstUpper
		while (i < len) {
			{
				srcChar = value(i).toInt
				if (srcChar.toChar >= Character.MIN_HIGH_SURROGATE && srcChar.toChar <= Character.MAX_HIGH_SURROGATE) {
					srcChar = codePointAt(i)
					srcCount = Character.charCount(srcChar)
				}
				else
				{
					srcCount = 1
				}
				if (localeDependent || srcChar == '\u03A3' || // GREEK CAPITAL LETTER SIGMA
					srcChar == '\u0130')
				{
					// LATIN CAPITAL LETTER I WITH DOT ABOVE
					lowerChar = ConditionalSpecialCasing.toLowerCaseEx(this, i, locale)
				}
				else
				{
					lowerChar = Character.toLowerCase(srcChar)
				}
				if ((lowerChar == Character.ERROR) || (lowerChar >= Character.MIN_SUPPLEMENTARY_CODE_POINT)) {
					if (lowerChar == Character.ERROR) {
						lowerCharArray = ConditionalSpecialCasing.toLowerCaseCharArray(this, i, locale)
					}
					else if (srcCount == 2) {
						resultOffset += Character.toChars(lowerChar, result, i + resultOffset) - srcCount
						continue //todo: continue is not supported
					}
					else
					{
						lowerCharArray = Character.toChars(lowerChar)
					}
					/* Grow result if needed */ val mapLen: Int = lowerCharArray.length
					if (mapLen > srcCount) {
						val result2: Array[Char] = new Array[Char](result.length + mapLen - srcCount)
						System.arraycopy(result, 0, result2, 0, i + resultOffset)
						result = result2
					}
					var x: Int = 0
					while (x < mapLen) {
						{
							result(i + resultOffset + x) = lowerCharArray(x)
						}
						{x += 1; x }
					}
					resultOffset += (mapLen - srcCount)
				}
				else
				{
					result(i + resultOffset) = lowerChar.toChar
				}
			}
			i += srcCount
		}
		return new String(result, 0, len + resultOffset)
	}
	
	/**
	  * Converts all of the characters in this {@code String} to lower
	  * case using the rules of the default locale. This is equivalent to calling
	  * {@code toLowerCase(Locale.getDefault())}.
	  * <p>
	  * <b>Note:</b> This method is locale sensitive, and may produce unexpected
	  * results if used for strings that are intended to be interpreted locale
	  * independently.
	  * Examples are programming language identifiers, protocol keys, and HTML
	  * tags.
	  * For instance, {@code "TITLE".toLowerCase()} in a Turkish locale
	  * returns {@code "t\u005Cu0131tle"}, where '\u005Cu0131' is the
	  * LATIN SMALL LETTER DOTLESS I character.
	  * To obtain correct results for locale insensitive strings, use
	  * {@code toLowerCase(Locale.ROOT)}.
	  * <p>
	  *
	  * @return the { @code String}, converted to lowercase.
	  * @see java.lang.String#toLowerCase(Locale)
	  */
	def toLowerCase: String = {
		return toLowerCase(Locale.getDefault)
	}
	
	/**
	  * Converts all of the characters in this {@code String} to upper
	  * case using the rules of the given {@code Locale}. Case mapping is based
	  * on the Unicode Standard version specified by the {@link java.lang.Character Character}
	  * class. Since case mappings are not always 1:1 char mappings, the resulting
	  * {@code String} may be a different length than the original {@code String}.
	  * <p>
	  * Examples of locale-sensitive and 1:M case mappings are in the following table.
	  *
	  * <table border="1" summary="Examples of locale-sensitive and 1:M case mappings. Shows Language code of locale, lower case, upper case, and description.">
	  * <tr>
	  * <th>Language Code of Locale</th>
	  * <th>Lower Case</th>
	  * <th>Upper Case</th>
	  * <th>Description</th>
	  * </tr>
	  * <tr>
	  * <td>tr (Turkish)</td>
	  * <td>&#92;u0069</td>
	  * <td>&#92;u0130</td>
	  * <td>small letter i -&gt; capital letter I with dot above</td>
	  * </tr>
	  * <tr>
	  * <td>tr (Turkish)</td>
	  * <td>&#92;u0131</td>
	  * <td>&#92;u0049</td>
	  * <td>small letter dotless i -&gt; capital letter I</td>
	  * </tr>
	  * <tr>
	  * <td>(all)</td>
	  * <td>&#92;u00df</td>
	  * <td>&#92;u0053 &#92;u0053</td>
	  * <td>small letter sharp s -&gt; two letters: SS</td>
	  * </tr>
	  * <tr>
	  * <td>(all)</td>
	  * <td>Fahrvergn&uuml;gen</td>
	  * <td>FAHRVERGN&Uuml;GEN</td>
	  * <td></td>
	  * </tr>
	  * </table>
	  *
	  * @param locale use the case transformation rules for this locale
	  * @return the { @code String}, converted to uppercase.
	  * @see java.lang.String#toUpperCase()
	  * @see java.lang.String#toLowerCase()
	  * @see java.lang.String#toLowerCase(Locale)
	  * @since 1.1
	  */
	def toUpperCase(locale: Locale): String = {
		if (locale == null) {
			throw new NullPointerException
		}
		var firstLower: Int = 0
		val len: Int = value.length
		/* Now check if there are any characters that need to be changed. */ scan //todo: labels is not supported
		{
			firstLower = 0
			while (firstLower < len) {
				{
					var c: Int = value(firstLower).toInt
					var srcCount: Int = 0
					if ((c >= Character.MIN_HIGH_SURROGATE) && (c <= Character.MAX_HIGH_SURROGATE)) {
						c = codePointAt(firstLower)
						srcCount = Character.charCount(c)
					}
					else
					{
						srcCount = 1
					}
					val upperCaseChar: Int = Character.toUpperCaseEx(c)
					if ((upperCaseChar == Character.ERROR) || (c != upperCaseChar)) {
						break scan // todo: label break is not supported
					}
					firstLower += srcCount
				}
			}
			return this
		}
		/* result may grow, so i+resultOffset is the write location in result */ var resultOffset: Int = 0
		var result: Array[Char] = new Array[Char](len) /* may grow */
		/* Just copy the first few upperCase characters. */ System.arraycopy(value, 0, result, 0, firstLower)
		val lang: String = locale.getLanguage
		val localeDependent: Boolean = ((lang eq "tr") || (lang eq "az") || (lang eq "lt"))
		var upperCharArray: Array[Char] = null
		var upperChar: Int = 0
		var srcChar: Int = 0
		var srcCount: Int = 0
		var i: Int = firstLower
		while (i < len) {
			{
				srcChar = value(i).toInt
				if (srcChar.toChar >= Character.MIN_HIGH_SURROGATE && srcChar.toChar <= Character.MAX_HIGH_SURROGATE) {
					srcChar = codePointAt(i)
					srcCount = Character.charCount(srcChar)
				}
				else
				{
					srcCount = 1
				}
				if (localeDependent) {
					upperChar = ConditionalSpecialCasing.toUpperCaseEx(this, i, locale)
				}
				else
				{
					upperChar = Character.toUpperCaseEx(srcChar)
				}
				if ((upperChar == Character.ERROR) || (upperChar >= Character.MIN_SUPPLEMENTARY_CODE_POINT)) {
					if (upperChar == Character.ERROR) {
						if (localeDependent) {
							upperCharArray = ConditionalSpecialCasing.toUpperCaseCharArray(this, i, locale)
						}
						else
						{
							upperCharArray = Character.toUpperCaseCharArray(srcChar)
						}
					}
					else if (srcCount == 2) {
						resultOffset += Character.toChars(upperChar, result, i + resultOffset) - srcCount
						continue //todo: continue is not supported
					}
					else
					{
						upperCharArray = Character.toChars(upperChar)
					}
					/* Grow result if needed */ val mapLen: Int = upperCharArray.length
					if (mapLen > srcCount) {
						val result2: Array[Char] = new Array[Char](result.length + mapLen - srcCount)
						System.arraycopy(result, 0, result2, 0, i + resultOffset)
						result = result2
					}
					var x: Int = 0
					while (x < mapLen) {
						{
							result(i + resultOffset + x) = upperCharArray(x)
						}
						{x += 1; x }
					}
					resultOffset += (mapLen - srcCount)
				}
				else
				{
					result(i + resultOffset) = upperChar.toChar
				}
			}
			i += srcCount
		}
		return new String(result, 0, len + resultOffset)
	}
	
	/**
	  * Converts all of the characters in this {@code String} to upper
	  * case using the rules of the default locale. This method is equivalent to
	  * {@code toUpperCase(Locale.getDefault())}.
	  * <p>
	  * <b>Note:</b> This method is locale sensitive, and may produce unexpected
	  * results if used for strings that are intended to be interpreted locale
	  * independently.
	  * Examples are programming language identifiers, protocol keys, and HTML
	  * tags.
	  * For instance, {@code "title".toUpperCase()} in a Turkish locale
	  * returns {@code "T\u005Cu0130TLE"}, where '\u005Cu0130' is the
	  * LATIN CAPITAL LETTER I WITH DOT ABOVE character.
	  * To obtain correct results for locale insensitive strings, use
	  * {@code toUpperCase(Locale.ROOT)}.
	  * <p>
	  *
	  * @return the { @code String}, converted to uppercase.
	  * @see java.lang.String#toUpperCase(Locale)
	  */
	def toUpperCase: String = {
		return toUpperCase(Locale.getDefault)
	}
	
	/**
	  * Returns a string whose value is this string, with any leading and trailing
	  * whitespace removed.
	  * <p>
	  * If this {@code String} object represents an empty character
	  * sequence, or the first and last characters of character sequence
	  * represented by this {@code String} object both have codes
	  * greater than {@code '\u005Cu0020'} (the space character), then a
	  * reference to this {@code String} object is returned.
	  * <p>
	  * Otherwise, if there is no character with a code greater than
	  * {@code '\u005Cu0020'} in the string, then a
	  * {@code String} object representing an empty string is
	  * returned.
	  * <p>
	  * Otherwise, let <i>k</i> be the index of the first character in the
	  * string whose code is greater than {@code '\u005Cu0020'}, and let
	  * <i>m</i> be the index of the last character in the string whose code
	  * is greater than {@code '\u005Cu0020'}. A {@code String}
	  * object is returned, representing the substring of this string that
	  * begins with the character at index <i>k</i> and ends with the
	  * character at index <i>m</i>-that is, the result of
	  * {@code this.substring(k, m + 1)}.
	  * <p>
	  * This method may be used to trim whitespace (as defined above) from
	  * the beginning and end of a string.
	  *
	  * @return A string whose value is this string, with any leading and trailing white
	  *         space removed, or this string if it has no leading or
	  *         trailing white space.
	  */
	def trim: String = {
		var len: Int = value.length
		var st: Int = 0
		val `val`: Array[Char] = value /* avoid getfield opcode */
		while ((st < len) && (`val`(st) <= ' ')) {
			{
				st += 1
			}
		}
		while ((st < len) && (`val`(len - 1) <= ' ')) {
			{
				len -= 1
			}
		}
		return if (((st > 0) || (len < value.length))) substring(st, len)
		       else this
	}
	
	/**
	  * This object (which is already a string!) is itself returned.
	  *
	  * @return the string itself.
	  */
	override def toString: String = {
		return this
	}
	
	/**
	  * Converts this string to a new character array.
	  *
	  * @return a newly allocated character array whose length is the length
	  *         of this string and whose contents are initialized to contain
	  *         the character sequence represented by this string.
	  */
	def toCharArray: Array[Char] = {
		// Cannot use Arrays.copyOf because of class initialization order issues
		val result: Array[Char] = new Array[Char](value.length)
		System.arraycopy(value, 0, result, 0, value.length)
		return result
	}
	
	/**
	  * Returns a formatted string using the specified format string and
	  * arguments.
	  *
	  * <p> The locale always used is the one returned by {@link
	  * java.util.Locale#getDefault() Locale.getDefault()}.
	  *
	  * @param  format
	  *         A <a href="../util/Formatter.html#syntax">format string</a>
	  *
	  * @param  args
	  *         Arguments referenced by the format specifiers in the format
	  *         string.  If there are more arguments than format specifiers, the
	  *         extra arguments are ignored.  The number of arguments is
	  *         variable and may be zero.  The maximum number of arguments is
	  *         limited by the maximum dimension of a Java array as defined by
	  *         <cite>The Java&trade; Virtual Machine Specification</cite>.
	  *         The behaviour on a
	  *         { @code null} argument depends on the <a
	  *                 href="../util/Formatter.html#syntax">conversion</a>.
	  *
	  * @throws  java.util.IllegalFormatException
	  * If a format string contains an illegal syntax, a format
	  * specifier that is incompatible with the given arguments,
	  * insufficient arguments given the format string, or other
	  * illegal conditions.  For specification of all possible
	  * formatting errors, see the <a
	  * href="../util/Formatter.html#detail">Details</a> section of the
	  * formatter class specification.
	  *
	  * @return A formatted string
	  *
	  * @see java.util.Formatter
	  * @since 1.5
	  */
	def format(format: String, args: Any*): String = {
		return new Formatter().format(format, args).toString
	}
	
	/**
	  * Returns a formatted string using the specified locale, format string,
	  * and arguments.
	  *
	  * @param  l
	  * The { @linkplain java.util.Locale locale} to apply during
	  *         formatting.  If { @code l} is { @code null} then no localization
	  *                                 is applied.
	  *
	  * @param  format
	  *         A <a href="../util/Formatter.html#syntax">format string</a>
	  *
	  * @param  args
	  *         Arguments referenced by the format specifiers in the format
	  *         string.  If there are more arguments than format specifiers, the
	  *         extra arguments are ignored.  The number of arguments is
	  *         variable and may be zero.  The maximum number of arguments is
	  *         limited by the maximum dimension of a Java array as defined by
	  *         <cite>The Java&trade; Virtual Machine Specification</cite>.
	  *         The behaviour on a
	  *         { @code null} argument depends on the
	  *                 <a href="../util/Formatter.html#syntax">conversion</a>.
	  *
	  * @throws  java.util.IllegalFormatException
	  * If a format string contains an illegal syntax, a format
	  * specifier that is incompatible with the given arguments,
	  * insufficient arguments given the format string, or other
	  * illegal conditions.  For specification of all possible
	  * formatting errors, see the <a
	  * href="../util/Formatter.html#detail">Details</a> section of the
	  * formatter class specification
	  *
	  * @return A formatted string
	  *
	  * @see java.util.Formatter
	  * @since 1.5
	  */
	def format(l: Locale, format: String, args: Any*): String = {
		return new Formatter(l).format(format, args).toString
	}
	
	/**
	  * Returns the string representation of the {@code Object} argument.
	  *
	  * @param   obj an { @code Object}.
	  * @return if the argument is { @code null}, then a string equal to
	  *                                    { @code "null"}; otherwise, the value of
	  *                                    { @code obj.toString()} is returned.
	  * @see java.lang.Object#toString()
	  */
	def valueOf(obj: Any): String = {
		return if ((obj == null)) "null"
		       else obj.toString
	}
	
	/**
	  * Returns the string representation of the {@code char} array
	  * argument. The contents of the character array are copied; subsequent
	  * modification of the character array does not affect the returned
	  * string.
	  *
	  * @param   data the character array.
	  * @return a { @code String} that contains the characters of the
	  *                   character array.
	  */
	def valueOf(data: Array[Char]): String = {
		return new String(data)
	}
	
	/**
	  * Returns the string representation of a specific subarray of the
	  * {@code char} array argument.
	  * <p>
	  * The {@code offset} argument is the index of the first
	  * character of the subarray. The {@code count} argument
	  * specifies the length of the subarray. The contents of the subarray
	  * are copied; subsequent modification of the character array does not
	  * affect the returned string.
	  *
	  * @param   data   the character array.
	  * @param   offset initial offset of the subarray.
	  * @param   count  length of the subarray.
	  * @return a { @code String} that contains the characters of the
	  *                   specified subarray of the character array.
	  * @exception IndexOutOfBoundsException if { @code offset} is
	  *                                                 negative, or { @code count} is negative, or
	  *                                                 { @code offset+count} is larger than
	  *                                                 { @code data.length}.
	  */
	def valueOf(data: Array[Char], offset: Int, count: Int): String = {
		return new String(data, offset, count)
	}
	
	/**
	  * Equivalent to {@link #valueOf(char[], int, int)}.
	  *
	  * @param   data   the character array.
	  * @param   offset initial offset of the subarray.
	  * @param   count  length of the subarray.
	  * @return a { @code String} that contains the characters of the
	  *                   specified subarray of the character array.
	  * @exception IndexOutOfBoundsException if { @code offset} is
	  *                                                 negative, or { @code count} is negative, or
	  *                                                 { @code offset+count} is larger than
	  *                                                 { @code data.length}.
	  */
	def copyValueOf(data: Array[Char], offset: Int, count: Int): String = {
		return new String(data, offset, count)
	}
	
	/**
	  * Equivalent to {@link #valueOf(char[])}.
	  *
	  * @param   data the character array.
	  * @return a { @code String} that contains the characters of the
	  *                   character array.
	  */
	def copyValueOf(data: Array[Char]): String = {
		return new String(data)
	}
	
	/**
	  * Returns the string representation of the {@code boolean} argument.
	  *
	  * @param   b a { @code boolean}.
	  * @return if the argument is { @code true}, a string equal to
	  *                                    { @code "true"} is returned; otherwise, a string equal to
	  *                                    { @code "false"} is returned.
	  */
	def valueOf(b: Boolean): String = {
		return if (b) "true"
		       else "false"
	}
	
	/**
	  * Returns the string representation of the {@code char}
	  * argument.
	  *
	  * @param   c a { @code char}.
	  * @return a string of length { @code 1} containing
	  *                                    as its single character the argument { @code c}.
	  */
	def valueOf(c: Char): String = {
		val data: Array[Char] = Array(c)
		return new String(data, true)
	}
	
	/**
	  * Returns the string representation of the {@code int} argument.
	  * <p>
	  * The representation is exactly the one returned by the
	  * {@code Integer.toString} method of one argument.
	  *
	  * @param   i an { @code int}.
	  * @return a string representation of the { @code int} argument.
	  * @see java.lang.Integer#toString(int, int)
	  */
	def valueOf(i: Int): String = {
		return Integer.toString(i)
	}
	
	/**
	  * Returns the string representation of the {@code long} argument.
	  * <p>
	  * The representation is exactly the one returned by the
	  * {@code Long.toString} method of one argument.
	  *
	  * @param   l a { @code long}.
	  * @return a string representation of the { @code long} argument.
	  * @see java.lang.Long#toString(long)
	  */
	def valueOf(l: Long): String = {
		return Long.toString(l)
	}
	
	/**
	  * Returns the string representation of the {@code float} argument.
	  * <p>
	  * The representation is exactly the one returned by the
	  * {@code Float.toString} method of one argument.
	  *
	  * @param   f a { @code float}.
	  * @return a string representation of the { @code float} argument.
	  * @see java.lang.Float#toString(float)
	  */
	def valueOf(f: Float): String = {
		return Float.toString(f)
	}
	
	/**
	  * Returns the string representation of the {@code double} argument.
	  * <p>
	  * The representation is exactly the one returned by the
	  * {@code Double.toString} method of one argument.
	  *
	  * @param   d a { @code double}.
	  * @return a  string representation of the { @code double} argument.
	  * @see java.lang.Double#toString(double)
	  */
	def valueOf(d: Double): String = {
		return Double.toString(d)
	}
	
	/**
	  * Returns a canonical representation for the string object.
	  * <p>
	  * A pool of strings, initially empty, is maintained privately by the
	  * class {@code String}.
	  * <p>
	  * When the intern method is invoked, if the pool already contains a
	  * string equal to this {@code String} object as determined by
	  * the {@link #equals(Object)} method, then the string from the pool is
	  * returned. Otherwise, this {@code String} object is added to the
	  * pool and a reference to this {@code String} object is returned.
	  * <p>
	  * It follows that for any two strings {@code s} and {@code t},
	  * {@code s.intern() == t.intern()} is {@code true}
	  * if and only if {@code s.equals(t)} is {@code true}.
	  * <p>
	  * All literal strings and string-valued constant expressions are
	  * interned. String literals are defined in section 3.10.5 of the
	  * <cite>The Java&trade; Language Specification</cite>.
	  *
	  * @return a string that has the same contents as this string, but is
	  *         guaranteed to be from a pool of unique strings.
	  */
	def intern: String
}
*/
