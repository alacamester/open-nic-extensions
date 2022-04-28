# open-nic-extensions
Open NIC shell firmware-extensions

## filter_simple : 
- Simple decoder and filter ; protocol stack: Eth/(VLAN)/IPv4/[TCP/UDP]
- Pass and drop filtering is implemented (evaluate drop first, then pass)
- Filter for: IPv4 SRC/DST address with netmask, IPv4 protocol value, TCP/UDP SRC/DST port value
- supports 256 filter-rules per-interface for now (can be extended further if needed)

### TODO : 
- add filter-precedence
- filter-cascading (data delay-chain) for better timing performance

## filter_simple_ipv6 : 
- Simple decoder and filter ; protocol stack: Eth/(VLAN)/[IPv4,IPv6]/[TCP/UDP]
- Pass and drop filtering is implemented (evaluate drop first, then pass)
- Filter for: IPv4 and IPv6 SRC/DST address with netmask, IPv4 and IPv6 protocol value, TCP/UDP SRC/DST port value
- supports 256 filter-rules per-interface for now (can be extended further if needed)
- IPv4 decoding and filtering is implemented using the well-known IPv4 prefix (0064:FF9B::)
- filters are cascaded (chained one after another) for better timing performance

### TODO:
- add filter-precedence
