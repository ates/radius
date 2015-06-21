%% Default port values
-define(ACCESS_REQUEST_PORT, 1812).
-define(ACCOUNTING_REQUEST_PORT, 1813).
-define(POD_COA_REQUEST_PORT, 3799).

%% RADIUS Packet format
%% 0                   1                   2                   3
%% 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |     Code      |  Identifier   |            Length             |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |                                                               |
%% |                         Authenticator                         |
%% |                                                               |
%% |                                                               |
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |  Attributes ...
-define(RADIUS_PACKET, Code:8, Ident:1/binary, Length:16, Auth:16/binary, Attrs/binary).

%% RADIUS packet record
%% Fields:
%%     code: Identifies the type of RADIUS packet
%%     identifier: Aids in matching requests and replies
%%     length: Indicates the length of the packet including the Code,
%%             Identifier, Length, Authenticator and Attribute fields
%%     authenticator: used to authenticate the reply from the RADIUS server,
%%                    and is used in the password hiding algorithm
%%     attributes: List of attributes that are required for the type of service,
%%                 as well as any desired optional attributes
-record(radius_packet, {code, ident, auth, attrs = []}).

%% RADIUS packet codes
-define(ACCESS_REQUEST, 1).
-define(ACCESS_ACCEPT, 2).
-define(ACCESS_REJECT, 3).
-define(ACCOUNTING_REQUEST, 4).
-define(ACCOUNTING_RESPONSE, 5).
-define(ACCESS_CHALLENGE, 11).
-define(DISCONNECT_REQUEST, 40).
-define(DISCONNECT_ACK, 41).
-define(DISCONNECT_NAK, 42).
-define(COA_REQUEST, 43).
-define(COA_ACK, 44).
-define(COA_NAK, 45).

%% The Acct-Status-Type attribute values
-define(ACCT_START, 1).
-define(ACCT_STOP, 2).
-define(INTERIM_UPDATE, 3).

%% RADIUS attribute format
%% 0                   1                   2
%% 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
%% |     Type      |    Length     |  Value ...
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
-define(ATTRIBUTE, Type:8, Length:8, Rest/binary).

%% Attribute record
%% Fields:
%%     code: Numeric code of the attribute
%%     type: Attribute type (octets/ipaddr/string/integer/date)
%%     name: Attribute name
-record(attribute, {code, type, name, opts}).

%% Value record
%% Fields:
%%     aname: Attribute name
%%     vname: Attribute value name
%%     value: Attribute value
-record(value, {aname, vname, value}).

%% Vendor-Specific attribute format
%% 0                   1                   2                   3
%% 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%% |     Type      |  Length       |            Vendor-Id
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
%%      Vendor-Id (cont)           |  String...
%% +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
-define(VENDOR_SPECIFIC, 26).
-define(VENDOR_ATTRIBUTE, Id:4/integer-unit:8, Type, Length, Rest/binary).

%% NAS specification record
%% Fields:
%%     ip: IP address or network of the NAS
%%     name: Identifies of NAS
%%     secret: Shared secret required for NAS authorization
-record(nas_spec, {
    name :: term(),
    ip :: {ip, inet:ip_address()} | {net, {inet:ip_address(), Mask :: 0..32 | inet:ip_address()}},
    secret :: string()
}).
