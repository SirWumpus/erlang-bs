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

Exports
-------

### ctype:isblank(Ch) -> boolean()
True if the character is a space or tab.

### ctype:isspace(Ch) -> boolean()
True if the character is a space, tab, carriage return, line feed, form feed, or vertical tab.

### ctype:isprint(Ch) -> boolean()
True if the character is a printable character, including space.

### ctype:isdigit(Ch) -> boolean()
True if the character is a decimal digit.

### ctype:isxdigit(Ch) -> boolean()
True if the character is a hexadecimal digit.

### ctype:iscntrl(Ch) -> boolean()
True if the characters is a control character.

### ctype:isupper(Ch) -> boolean()
True if the character is an upper case character.

### ctype:islower(Ch) -> boolean()
True if the character is an lower case character.

### ctype:isalpha(Ch) -> boolean()
True if the character is an alphabetic character.

### ctype:isalnum(Ch) -> boolean()
True if the character is an alphabetic or numeric character.

### ctype:ispunct(Ch) -> boolean()
True if the character is an punctuation character.

### str:at(Bs, Index) -> Byte() | badarg
Return the character/byte at the given index in the binary string.

### str:cat(Bs1, Bs2) -> Bs
Concatenate two binary strings.

### str:chop(Bs) -> Bs
Remove leading whitespace from a binary string.

### str:chr(Bs, Ch) ->  Index | -1
Return index of first occurrence of character in the binary string; otherwise -1 if not found.

### str:cmp(Bs1, Bs2) -> integer() 
Return an integer greater than, equal to, or less than 0 according to whether binary string Bs1 is greater than, equal to, or less than binary string Bs2.

### str:cpy(Bs) -> Bs
Return a copy of Bs.  Alias for `binary:copy/1`.

### str:cspn(Bs, Delims) -> Length
Return the number of leading characters in the binary string before any of the delimiters are found.

### str:len(Bs) -> Length
Length of binary string.  Alias for `byte_size/1`.

### str:ncmp(Bs1, Bs2, Length) -> integer() 
Return an integer greater than, equal to, or less than 0 according to whether binary string Bs1 is greater than, equal to, or less than binary string Bs2, comparing at most Length octets.

### str:ncat(Bs1, Bs2, Length) -> Bs
Append Length characters from Bs2 to Bs1.

### str:ncpy(Bs, Length) -> Bs
Return a copy of the first Length octets of Bs. 

### str:rchop(Bs) ->  Bs
Remove trailing whitespace from a binary string.

### str:rchr(Bs, Ch) ->  Index | -1
Return index of last occurrence of character in the binary string; otherwise -1 if not found.

### str:rev(Bs) -> Bs
Reverse the binary string.

### str:spn(Bs, Delims) -> Length
Return the number of leading delimiters in the binary string.

### str:sub(Bs, Start) -> Bs  
Return the binary substring from starting index until  end of string.  The index counts from zero (0).

### str:sub(Bs, Start, Stop) -> Bs
Return the binary substring between start and stop index, excluding stop.  The indices counts from zero (0).

### str:tok(Bs, Delims) -> {<< Token >>, << Rest >>}
Return a tuple of the first token separated by one or more delimiters and the remaing binary string.

### str:trim(Bs) -> Bs
Remove leading and trailing whitespace from a binary string.


Copyright
---------

Copyright 2017 by Anthony Howe.  All rights reserved.


MIT License
-----------

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
