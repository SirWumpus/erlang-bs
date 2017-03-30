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


Exports
-------

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
Use binary string Fmt to format Date and Time into Bs.  All ordinary characters are copied as-is, while the following format characters are replaced (similar to strftime(3)).  

**%A**	is replaced by the ~~locale's~~ English full weekday name.

**%a**	is replaced by the ~~locale's~~ English abbreviated weekday name.

**%B**	is replaced by the ~~locale's~~ English full month name.

**%b** or **%h** is replaced by the ~~locale's~~ English abbreviated month name.

**%C**	is replaced by the century (a year divided by 100 and truncated to an integer) as a decimal number [00,99].

**%c**	is replaced by the locale's appropriate date and time representation.

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

**%p**	is replaced by the locale's equivalent of either ``AM'' or ``PM''.

**%R**	is replaced by the time in the format ``%H:%M''.

**%r**	is replaced by the locale's representation of 12-hour clock time using AM/PM notation.

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
### str:tok(Bs, Delims) -> {<< Token >>, << Rest >>}
Return a tuple of the first token separated by one or more delimiters and the remaing binary string.

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
