# open-nic-extensions
Open NIC shell firmware-extensions

## filter_simple : 
- Simple decoder and filter ; protocol stack: Eth/(VLAN)/IPv4/[TCP/UDP]
- Only pass-filtering is implemented
- Filter for: IPv4 SRC/DST address, IPv4 protocol value, TCP/UDP SRC/DST port value
- supports 128 filter-rules per-interface for now (can be extended to 256)

### TODO : 
- add support for IPv6 filtering
- add filter-precedence
- add drop-function (now its pass-only)