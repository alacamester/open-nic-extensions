# open-nic-extensions
Open NIC shell firmware-extensions

## filter_simple : 
- Simple decoder and filter ; protocol stack: Eth/(VLAN)/IPv4/[TCP/UDP]
- Pass and drop filtering is implemented (evaluate drop first, then pass)
- Filter for: IPv4 SRC/DST address with netmask, IPv4 protocol value, TCP/UDP SRC/DST port value
- supports 256 filter-rules per-interface for now (can be extended further if needed)

### TODO : 
- add support for IPv6 filtering
- add filter-precedence
- filter-cascading (data delay-chain) for better timing performance