RADIUS protocol
===============

Supported data types:
---------------------

 * string - 0-253 octets
 * ipaddr - 4 octets in network byte order
 * integer - 32 bit value in big endian order (high byte first)
 * date - 32 bit value in big endian order - seconds since 00:00:00 GMT,  Jan.  1,  1970
 * ipv6addr - 16 octets in network byte order
 * ipv6prefix - 18 octets in network byte order
 * byte - 8 bit unsigned integer
 * octets - raw octets, printed and input as hex strings
