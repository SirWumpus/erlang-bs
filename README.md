Binary String Support
=====================

C-like string and ctype functions for Erlang binary strings.


Data Types
----------

* Bs = <<"...">>
* Ch = ascii().
* Delims = <<"...">>
* Index = integer() >= 0
* Length = integer() >= 0
* Date = { Year, Month, Day }
* Time = { Hour, Minute, Second }
* Tz = integer() ; time zone offset in seconds, eg. -18000 = -05:00 = EST, -12600 = -03:30 = Newfoundland


Exports
-------

### ctype:isbase(Ch, Base) -> boolean()
True if the character is in the given number base (2..36).

- - -

### ctype:isblank(Ch) -> boolean()
True if the character is a space or tab.

- - -
### ctype:isspace(Ch) -> boolean()
True if the character is a space, tab, carriage return, line feed, form feed, or vertical tab.

- - -
### ctype:isprint(Ch) -> boolean()
True if the character is a printable character, including space.

- - -
### ctype:isdigit(Ch) -> boolean()
True if the character is a decimal digit.

- - -
### ctype:isxdigit(Ch) -> boolean()
True if the character is a hexadecimal digit.

- - -
### ctype:iscntrl(Ch) -> boolean()
True if the characters is a control character.

- - -
### ctype:isupper(Ch) -> boolean()
True if the character is an upper case character.

- - -
### ctype:islower(Ch) -> boolean()
True if the character is an lower case character.

- - -
### ctype:isalpha(Ch) -> boolean()
True if the character is an alphabetic character.

- - -
### ctype:isalnum(Ch) -> boolean()
True if the character is an alphabetic or numeric character.

- - -
### ctype:ispunct(Ch) -> boolean()
True if the character is an punctuation character.

- - -
### ctype:tolower(Ch) -> boolean()
If the character is alphabetic, return its corresponding lower case letter; otherwise the character as-is.

- - -
### ctype:toupper(Ch) -> boolean()
If the character is alphabetic, return its corresponding upper case letter; otherwise the character as-is.

- - -
### str:at(Bs, Index) -> byte() | badarg
Return the character/byte at the given index in the binary string.

- - -
### str:cat(Bs1, Bs2) -> Bs
Concatenate two binary strings.

- - -
### str:chr(Bs, Ch) ->  Index | -1
Return index of first occurrence of character in the binary string; otherwise -1 if not found.

- - -
### str:casecmp(Bs1, Bs2) -> integer()
Return an integer greater than, equal to, or less than 0 according to whether caseless binary string Bs1 is greater than, equal to, or less than caseless binary string Bs2.

- - -
### str:cmp(Bs1, Bs2) -> integer() 
Return an integer greater than, equal to, or less than 0 according to whether binary string Bs1 is greater than, equal to, or less than binary string Bs2.

- - -
### str:cpy(Bs) -> Bs
Return a copy of Bs.  Alias for `binary:copy/1`.

- - -
### str:cspn(Bs, Delims) -> Length
Return the number of leading characters in the binary string before any of the delimiters are found.

- - -
### str:error(Reason) -> Bs
Return binary string error message for Reason.

- - -
### str:ftime(Fmt, {Date, Time}) -> Bs
### str:ftime(Fmt, {Date, Time, Tz}) -> Bs
Use binary string Fmt to format Date and Time into Bs.  All ordinary characters are copied as-is, while the following format characters are replaced (similar to strftime(3)).  Without Tz, assumes local time of user $TZ or system when $TZ is unset.  To ensure UTC, use ftime/2 with Tz = 0.

**%A**	is replaced by the ~~locale's~~ English full weekday name.

**%a**	is replaced by the ~~locale's~~ English abbreviated weekday name.

**%B**	is replaced by the ~~locale's~~ English full month name.

**%b** or **%h** is replaced by the ~~locale's~~ English abbreviated month name.

**%C**	is replaced by the century (a year divided by 100 and truncated to an integer) as a decimal number [00,99].

**%c**	is replaced by the ~~locale's~~ RFC appropriate date and time representation, ``%e %b %Y %H:%M:%S''.

**%D**	is replaced by the date in the format ``%m/%d/%y''.

**%d**	is replaced by the day of the month as a decimal number [01,31].

**%e**	is replaced by the day of month as a decimal number [1,31]; single digits are preceded by a blank.

**%F**	is replaced by the date in the format ``%Y-%m-%d'' (the ISO 8601 date format).

**%H**	is replaced by the hour (24-hour clock) as a decimal number [00,23].

**%I**	is replaced by the hour (12-hour clock) as a decimal number [01,12].

**%j**	is replaced by the day of the year as a decimal number [001,366].

**%k**	is replaced by the hour (24-hour clock) as a decimal number [0,23]; single digits are preceded by a blank.

**%l**	is replaced by the hour (12-hour clock) as a decimal number [1,12]; single digits are preceded by a blank.

**%M**	is replaced by the minute as a decimal number [00,59].

**%m**	is replaced by the month as a decimal number [01,12].

**%n**	is replaced by a newline.

**%p**	is replaced by ~~the locale's equivalent of~~ either "am" or "pm".

**%R**	is replaced by the time in the format ``%H:%M''.

**%r**	is replaced by the ~~locale's~~ representation of 12-hour clock time using AM/PM notation.

**%s**	is replaced by the number of seconds since the Epoch.

**%T**	is replaced by the time in the format ``%H:%M:%S''.

**%t**	is replaced by a tab.

**%V**	is replaced by the week number of the year (Monday as the first day of the week) as a decimal number [01,53]. According to ISO 8601 the week containing January 1 is week 1 if it has four or more days in the new year, otherwise it is week 53 of the previous year, and the next week is week 1.

**%v**	is replaced by the date in the format ``%e-%b-%Y''.

**%Y**	is replaced by the year with century as a decimal number.

**%y**	is replaced by the year without century as a decimal number [00,99].

**%z**	is replaced by the offset from the Prime Meridian in the format +HHMM or -HHMM (ISO 8601) as appropriate, with positive values representing locations east of Greenwich, or by the empty string if this is not determinable.  ``[-]hhmm''.

**%%**	is replaced by `%'.


- - -
### str:len(Bs) -> Length
Length of binary string.  Alias for `byte_size/1`.

- - -
### str:lpad(Bs, Pad, Width) -> Bs
Return Bs padded with Pad characters to the left until at least Width.

- - -
### str:ltrim(Bs) -> Bs
Remove leading whitespace from a binary string.

- - -
### str:lower(Bs) -> Bs
Return a binary string converted to lower case.

- - -
### str:ncasecmp(Bs1, Bs2, Length) -> integer() 
Return an integer greater than, equal to, or less than 0 according to whether caseless binary string Bs1 is greater than, equal to, or less than caseless binary string Bs2, comparing at most Length octets.

- - -
### str:ncat(Bs1, Bs2, Length) -> Bs
Append Length characters from Bs2 to Bs1.

- - -
### str:ncmp(Bs1, Bs2, Length) -> integer()
Return an integer greater than, equal to, or less than 0 according to whether binary string Bs1 is greater than, equal to, or less than binary string Bs2, comparing at most Length octets.

- - -
### str:pad_int(Int, Pad, Width) -> Bs
Return a binary string with the decimal integer right justified to the minimum field width; numbers shorter than the field width are left padded.  If the integer is negative and Pad is the zero (0) character, then a minus sign appears ahead of the zero padding.  Positive numbers have no sign.

- - -
### str:pad_sign_int(Int, Pad, Width) -> Bs
Return a binary string with the signed decimal integer right justified to the minimum field width; numbers shorter than the field width are left padded.  If Pad is the zero (0) character, then the plus or minus sign appears ahead of the zero padding.

- - -
### str:ptime(Bs, Fmt) -> { {Date, Time, Tz}, << Rest >> } | {badarg, << Rest >>}

Whitespace matches zero or more whitespace characters and ordinary characters match themselves.

**%a** the day of week, using ~~the locale's~~ English weekday names; either the abbreviated or full name may be specified.

**%A** the same as %a.

**%b** the month, using ~~the locale's~~ English month names; either the abbreviated or full name may be specified.

**%B** the same as %b.

**%c** the date and time, using ~~the locale's date and time format~~ ``%e %b %Y %H:%M:%S''.

**%C** the century number [0,99]; leading zeros are permitted but not required.  This conversion should be used in conjunction with the %y conversion.

**%d** the day of month [1,31]; leading zeros are permitted but not required.

**%D** the date as %m/%d/%y.

**%e** the same as %d.

**%F** the date as %Y-%m-%d (the ISO 8601 date format).

**%h** the same as %b.

**%H** the hour (24-hour clock) [0,23]; leading zeros are permitted but not required.

**%I** the hour (12-hour clock) [1,12]; leading zeros are permitted but not required.

**%j** the day number of the year [1,366]; leading zeros are permitted but not required.

**%k** the same as %H.

**%l** the same as %I.

**%m** the month number [1,12]; leading zeros are permitted but not required.

**%M** the minute [0,59]; leading zeros are permitted but not required.

**%n** any white-space, including none.

**%p** ~~the locale's equivalent of~~ a.m. or p.m.

**%r** the time (12-hour clock) with %p, ~~using the locale's time format~~ ``%l:%M %p''.

**%R** the time as %H:%M.

**%S** the seconds [0,61]; leading zeros are permitted but not required.

**%s** the number of seconds since the Epoch, UTC (see mktime(3)).

**%t** any white-space, including none.

**%T** the time as %H:%M:%S.

**%y** the year within the 20th century [69,99] or the 21st century [0,68]; leading zeros are permitted but not required.  If specified in conjunction with %C, specifies the year [0,99] within that century.

**%Y** the year, including the century (i.e., 1996).

**%z** an ISO 8601 or RFC-2822 timezone specification.  This is one of the following: the offset from Universal Time Coordinate (`UTC') specified as: "[+-]hh[:]mm".

**%%** matches a literal `%'.  No argument is converted.

- - -
### str:rchr(Bs, Ch) ->  Index | -1
Return index of last occurrence of character in the binary string; otherwise -1 if not found.

- - -
### str:rpad(Bs, Pad, Width) -> Bs
Return Bs padded with Pad characters to the right until at least Width.

- - -
### str:ncpy(Bs, Length) -> Bs
Return a copy of the first Length octets of Bs. 

- - -
### str:rev(Bs) -> Bs
Reverse the binary string.

- - -
### str:rtrim(Bs) ->  Bs
Remove trailing whitespace from a binary string.

- - -
### str:spn(Bs, Delims) -> Length
Return the number of leading delimiters in the binary string.

- - -
### str:sub(Bs, Start) -> Bs  
Return the binary substring from starting index until  end of string.  The index counts from zero (0).

- - -
### str:sub(Bs, Start, Stop) -> Bs
Return the binary substring between start and stop index, excluding stop.  The indices counts from zero (0).

- - -
### str:to_date_time(Bs) -> {{Date, Time, Tz}, << Rest >>} | badarg
Attempt to parse the leading portion of Bs as an ISO 8601, RFC 2822, or ctime() date-time string.  If time zone information is missing, then the local time zone is assumed.  `badarg` is returned if no input is consumed.

- - -
### str:to_int(Bs, Base) -> { integer(), << Rest >> } | badarg
Return a tulpe of the leading parsed integer and remaining binary string.  The parsed integer string can be  padded with leading zeros.  If base is zero or 16, the string may then include a '0x' prefix, and the number will be read in base 16; otherwise, a zero base is taken as 10 (decimal) unless the next character is '0', in which case it is taken as 8 (octal).  `badarg` is returned if no input is consumed.

- - -
### str:tok(Bs, Delims) -> {<< Token >>, << Rest >>}
Return a tuple of the first token separated by one or more delimiters and the remaining binary string.

- - -
### str:tr(Bs, FromSet) -> Bs
### str:tr(Bs, FromSet, ToSet) -> Bs
For each character in Bs found at position N of FromSet (a binary string) is replaced by a character at position N of ToSet (a binary string); if ToSet is shorter than FromSet, then the last character of ToSet is used.  If ToSet is missing or empty, then characters in FromSet are deleted from Bs.

- - -
### str:trim(Bs) -> Bs
Remove leading and trailing whitespace from a binary string.

- - -
### str:upper(Bs) -> Bs
Return a binary string converted to upper case.


Copyright
---------

Copyright 2017 by Anthony Howe.  All rights reserved.


MIT License
-----------

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
