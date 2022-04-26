library IEEE;
use IEEE.std_logic_1164.all;
use work.tuw.all;
--      ==========
PACKAGE pkg_DECmod100G IS
--      ==========
CONSTANT IP46_PREFIX 	: STD_LOGIC_VECTOR(128-32-1 downto 0) := X"00000000000000009BFF6400";

  COMPONENT Data2Nibble
        PORT (
             Clk 		: IN   STD_LOGIC;       -- Clock
--
             DATA_IN 	: IN   STD_LOGIC_VECTOR(512-1 downto 0); -- Data In
             SOP_IN 	: IN   STD_LOGIC;      
             DAV_IN   : IN   STD_LOGIC;
             EOP_IN 	: IN   STD_LOGIC;      
             M64_IN 	: IN   STD_LOGIC_VECTOR(6-1 downto 0);
--             
             DATA_OUT	: OUT  STD_LOGIC_VECTOR(2*512-1 downto 0);
             AV_OUT 	: OUT  STD_LOGIC;
             SOP_OUT   : OUT  STD_LOGIC; -- is output delayed
             M64_OUT 	: OUT  STD_LOGIC_VECTOR(7-1 downto 0)
             );
  END COMPONENT;
--
  COMPONENT DEC_ETH
        PORT (
             Clk 		: IN   STD_LOGIC;       -- Clock
--
             DATA_IN 	: IN   STD_LOGIC_VECTOR(2*512-1 downto 0); -- Data In
             AV_IN 		: IN   STD_LOGIC;      
             M64_IN 	: IN   STD_LOGIC_VECTOR(7-1 downto 0);
--             
             DATA_OUT	: OUT  STD_LOGIC_VECTOR(2*512-1 downto 0);
             AV_OUT 	: OUT  STD_LOGIC;
             M64_OUT 	: OUT  STD_LOGIC_VECTOR(7-1 downto 0);
             IPv4_Av_OUT: OUT  STD_LOGIC;
             IPv6_Av_OUT: OUT  STD_LOGIC
             );
  END COMPONENT;
--
  COMPONENT DEC_IP
        PORT (
             Clk 		: IN   STD_LOGIC;       -- Clock
--
             DATA_IN 	: IN   STD_LOGIC_VECTOR(2*512-1 downto 0); -- Data In
             AV_IN 		: IN   STD_LOGIC;
             En_IN 		: IN   STD_LOGIC_VECTOR(2-1 downto 0);   
             M64_IN 	: IN   STD_LOGIC_VECTOR(7-1 downto 0);
--             
             DATA_OUT	: OUT  STD_LOGIC_VECTOR(2*512-1 downto 0);
             AV_OUT 	: OUT  STD_LOGIC;
             En_OUT     : OUT  STD_LOGIC;
             M64_OUT 	: OUT  STD_LOGIC_VECTOR(7-1 downto 0);
             IP_SRC_OUT : OUT  STD_LOGIC_VECTOR(16*8-1 downto 0);
             IP_DST_OUT : OUT  STD_LOGIC_VECTOR(16*8-1 downto 0);
             PROTO_OUT  : OUT  STD_LOGIC_VECTOR(8-1 downto 0);
             UDP_Av_OUT : OUT  STD_LOGIC;
             TCP_Av_OUT : OUT  STD_LOGIC
             );
  END COMPONENT;
--
  COMPONENT DEC_L4
        PORT (
             Clk 		: IN   STD_LOGIC;       -- Clock
--
             DATA_IN 	: IN   STD_LOGIC_VECTOR(2*512-1 downto 0); -- Data In
             AV_IN 		: IN   STD_LOGIC;
             En_UDP_IN 	: IN   STD_LOGIC;   
             En_TCP_IN 	: IN   STD_LOGIC;   
             M64_IN 	: IN   STD_LOGIC_VECTOR(7-1 downto 0);
--             
             DATA_OUT	: OUT  STD_LOGIC_VECTOR(2*512-1 downto 0);
             AV_OUT 	: OUT  STD_LOGIC;
             En_OUT     : OUT  STD_LOGIC;             
             M64_OUT 	: OUT  STD_LOGIC_VECTOR(7-1 downto 0);
             L4_SRC_OUT : OUT  STD_LOGIC_VECTOR(2*8-1 downto 0);
             L4_DST_OUT : OUT  STD_LOGIC_VECTOR(2*8-1 downto 0);
             UDP_LEN_OUT  : OUT  STD_LOGIC_VECTOR(2*8-1 downto 0);
             UDP_DATA_OUT : OUT  STD_LOGIC_VECTOR(128-1 downto 0);
             UDP_Av_OUT : OUT  STD_LOGIC;
             TCP_Av_OUT : OUT  STD_LOGIC
             );
  END COMPONENT;


END pkg_DECmod100G;

library IEEE;
use IEEE.std_logic_1164.all;
USE IEEE.std_logic_UNSIGNED.ALL;
USE work.tuw.all;
--

ENTITY Data2Nibble IS
        PORT (
             Clk 		: IN   STD_LOGIC;       -- Clock
--
             DATA_IN 	: IN   STD_LOGIC_VECTOR(512-1 downto 0); -- Data In
             SOP_IN 	: IN   STD_LOGIC;      
             DAV_IN   : IN   STD_LOGIC;
             EOP_IN 	: IN   STD_LOGIC;      
             M64_IN 	: IN   STD_LOGIC_VECTOR(6-1 downto 0);
--             
             DATA_OUT	: OUT  STD_LOGIC_VECTOR(2*512-1 downto 0);
             AV_OUT 	: OUT  STD_LOGIC;
             SOP_OUT   : OUT  STD_LOGIC; -- is output delayed
             M64_OUT 	: OUT  STD_LOGIC_VECTOR(7-1 downto 0)
             );
END Data2Nibble;

ARCHITECTURE STRUCTURE OF Data2Nibble IS 
--
SIGNAL Is_2Nibble		: STD_LOGIC := '0';
--
BEGIN

UIn: PROCESS(clk, SOP_IN, EOP_IN, DAV_IN)
BEGIN
IF (Clk'event AND Clk = '1') THEN
SOP_OUT     <= SOP_IN;
IF (SOP_IN = '1') THEN
	IF (EOP_IN = '0') THEN
		Is_2Nibble	<= '1';
		AV_OUT		  <= '0';
	ELSE
		Is_2Nibble	<= '0';
		AV_OUT		  <= '1';
	END IF;
	DATA_OUT(1*512-1 downto 0*512) 	<= DATA_IN;
	M64_OUT						<= '0'&M64_IN;
ELSE
  DATA_OUT(2*512-1 downto 1*512) 	<= DATA_IN;
  IF (DAV_IN = '1') THEN
	   Is_2Nibble				<= '0';
	   AV_OUT						<= Is_2Nibble;
  ELSE
     AV_OUT           <= '0';
  END IF;
	M64_OUT						<= '1'&M64_IN;
END IF;
END IF;
END PROCESS;

END STRUCTURE;

--
---- ETH
--

library IEEE;
use IEEE.std_logic_1164.all;
USE IEEE.std_logic_UNSIGNED.ALL;
USE work.tuw.all;
--

ENTITY DEC_ETH IS
        PORT (
             Clk 		: IN   STD_LOGIC;       -- Clock
--
             DATA_IN 	: IN   STD_LOGIC_VECTOR(2*512-1 downto 0); -- Data In
             AV_IN 		: IN   STD_LOGIC;      
             M64_IN 	: IN   STD_LOGIC_VECTOR(7-1 downto 0);
--             
             DATA_OUT	: OUT  STD_LOGIC_VECTOR(2*512-1 downto 0);
             AV_OUT 	: OUT  STD_LOGIC;
             M64_OUT 	: OUT  STD_LOGIC_VECTOR(7-1 downto 0);
             IPv4_Av_OUT: OUT  STD_LOGIC;
             IPv6_Av_OUT: OUT  STD_LOGIC
             );
END DEC_ETH;

ARCHITECTURE STRUCTURE OF DEC_ETH IS 
--
SIGNAL Av						: STD_LOGIC;
SIGNAL M64 					: STD_LOGIC_VECTOR(7-1 downto 0) := (others => '0');
SIGNAL DATA 				: STD_LOGIC_VECTOR(2*512-1 downto 0);
SIGNAL Is_VLAN			: STD_LOGIC;
SIGNAL Is_IPv4			: STD_LOGIC_VECTOR(2-1 downto 0);
SIGNAL Is_IPv6			: STD_LOGIC_VECTOR(2-1 downto 0);

SIGNAL Av_r1 				: STD_LOGIC;
SIGNAL DATA_r1			: STD_LOGIC_VECTOR(2*512-1 downto 0);
SIGNAL M64_r1				: STD_LOGIC_VECTOR(7-1 downto 0) := (others => '0');
SIGNAL IPv4_r1			: STD_LOGIC;
SIGNAL IPv6_r1			: STD_LOGIC;
SIGNAL POS_next_r1	: STD_LOGIC_VECTOR(7-1 downto 0)  := (others => '0');

CONSTANT PROTO_ETH_IPv4     : STD_LOGIC_VECTOR(16-1 downto 0) := X"0008";
CONSTANT PROTO_ETH_IPv6     : STD_LOGIC_VECTOR(16-1 downto 0) := X"DD86";
CONSTANT PROTO_ETH_VLAN     : STD_LOGIC_VECTOR(16-1 downto 0) := X"0081";
--
BEGIN

-- + 1D
UCmp: PROCESS(clk)
BEGIN
IF (clk'event AND clk = '1') THEN
Av		<= Av_IN;
M64		<= M64_IN;
DATA 	<= DATA_IN;
-- IPv4
IF (DATA_IN(14*8-1 downto 12*8) = PROTO_ETH_IPv4) THEN
	Is_IPv4(0)		<= '1';
ELSE
	Is_IPv4(0)		<= '0';
END IF;
-- IPv6
IF (DATA_IN(14*8-1 downto 12*8) = PROTO_ETH_IPv6) THEN
	Is_IPv6(0)		<= '1';
ELSE
	Is_IPv6(0)		<= '0';
END IF;
-- VLAN
IF (DATA_IN(14*8-1 downto 12*8) = PROTO_ETH_VLAN) THEN
	Is_VLAN			<= '1';
ELSE
	Is_VLAN			<= '0';
END IF;
-- VLAN - IPv4
IF (DATA_IN((14+4)*8-1 downto (12+4)*8) = PROTO_ETH_VLAN) THEN
	Is_IPv4(1)		<= '1';
ELSE
	Is_IPv4(1)		<= '0';
END IF;
-- VLAN - IPv6
IF (DATA_IN((14+4)*8-1 downto (12+4)*8) = PROTO_ETH_VLAN) THEN
	Is_IPv6(1)		<= '1';
ELSE
	Is_IPv6(1)		<= '0';
END IF;
--
END IF;
END PROCESS;

-- +1D
UOut: PROCESS(clk)
BEGIN
IF (clk'event AND clk = '1') THEN
--
Av_r1 		<= Av;
DATA_r1		<= DATA;
M64_r1      <= M64;
IF (Av = '1') THEN
	IPv4_r1 	<= Is_IPv4(0) OR (Is_VLAN AND Is_IPv4(1));
	IPv6_r1 	<= Is_IPv6(0) OR (Is_VLAN AND Is_IPv6(1));
	IF (Is_IPv4(0) = '1' OR Is_IPv6(0) = '1') THEN
		POS_next_r1 	<= "0001110"; -- shift by 14 bytes
	ELSIF (Is_VLAN = '1' AND (Is_IPv4(1) = '1' OR Is_IPv6(1) = '1')) THEN
		POS_next_r1 	<= "0010010"; -- shift by 18 bytes
	END IF;
ELSE
	IPv4_r1 	<= '0';
	IPv6_r1 	<= '0';
END IF;
--
IPv4_Av_OUT 	<= IPv4_r1;
IPv6_Av_OUT 	<= IPv6_r1;
Av_OUT		    <= Av_r1;
M64_OUT 	    <= M64_r1 - POS_next_r1;
CASE POS_next_r1 IS
	WHEN "0001110" => -- 14
		DATA_OUT(2*512-14*8-1 downto 0) 	<= DATA_r1(2*512-1 downto 14*8);
	WHEN "0010010" => -- 18
		DATA_OUT(2*512-18*8-1 downto 0) 	<= DATA_r1(2*512-1 downto 18*8);
	WHEN OTHERS =>
        NULL;
--		DATA_OUT 	<= DATA_r1;
END CASE;
--
END IF;
END PROCESS;

END STRUCTURE;

--
---- IPv4
--

library IEEE;
use IEEE.std_logic_1164.all;
USE IEEE.std_logic_UNSIGNED.ALL;
USE work.pkg_DECmod100G.IP46_PREFIX;
USE work.tuw.all;
--

ENTITY DEC_IP IS
        PORT (
             Clk 		: IN   STD_LOGIC;       -- Clock
--
             DATA_IN 	: IN   STD_LOGIC_VECTOR(2*512-1 downto 0); -- Data In
             AV_IN 		: IN   STD_LOGIC;
             En_IN 		: IN   STD_LOGIC_VECTOR(2-1 downto 0);   
             M64_IN 	: IN   STD_LOGIC_VECTOR(7-1 downto 0);
--             
             DATA_OUT	: OUT  STD_LOGIC_VECTOR(2*512-1 downto 0);
             AV_OUT 	: OUT  STD_LOGIC;
             En_OUT     : OUT  STD_LOGIC;
             M64_OUT 	: OUT  STD_LOGIC_VECTOR(7-1 downto 0);
             IP_SRC_OUT : OUT  STD_LOGIC_VECTOR(16*8-1 downto 0);
             IP_DST_OUT : OUT  STD_LOGIC_VECTOR(16*8-1 downto 0);
             PROTO_OUT  : OUT  STD_LOGIC_VECTOR(8-1 downto 0);
             UDP_Av_OUT : OUT  STD_LOGIC;
             TCP_Av_OUT : OUT  STD_LOGIC
             );
END DEC_IP;

ARCHITECTURE STRUCTURE OF DEC_IP IS 
--
SIGNAL Av				: STD_LOGIC;
SIGNAL M64 				: STD_LOGIC_VECTOR(7-1 downto 0);
SIGNAL DATA 			: STD_LOGIC_VECTOR(2*512-1 downto 0);
SIGNAL IPv4_OK			: STD_LOGIC;
SIGNAL IPv6_OK			: STD_LOGIC;
SIGNAL IP_SRC			: STD_LOGIC_VECTOR(16*8-1 downto 0);
SIGNAL IP_DST			: STD_LOGIC_VECTOR(16*8-1 downto 0);
SIGNAL IP_PROTO			: STD_LOGIC_VECTOR(8-1 downto 0);
SIGNAL Is_UDP 			: STD_LOGIC;
SIGNAL Is_TCP 			: STD_LOGIC;
SIGNAL M64_Decr4 		: STD_LOGIC_VECTOR(7-1 downto 0);
SIGNAL M64_Decr6 		: STD_LOGIC_VECTOR(7-1 downto 0);

CONSTANT IP4_HEADER_LEN  : STD_LOGIC_VECTOR(7-1 downto 0) := "0010100"; --20
CONSTANT IP6_HEADER_LEN  : STD_LOGIC_VECTOR(7-1 downto 0) := "0101000"; --40
CONSTANT IP_PROTO_UDP   : STD_LOGIC_VECTOR(8-1 downto 0) := X"11";
CONSTANT IP_PROTO_TCP   : STD_LOGIC_VECTOR(8-1 downto 0) := X"06";
--
BEGIN

-- + 2D
UShd: PROCESS(clk)
BEGIN
IF (clk'event AND clk = '1') THEN
Av		<= Av_IN;
M64		<= M64_IN;
DATA 	<= DATA_IN;
-- IPv4
IF (Av_IN = '1') THEN
CASE (En_IN) IS
	WHEN "01" =>
		IF (M64_IN >= IP4_HEADER_LEN AND DATA_IN(1*8-1 downto 4) = X"4") THEN -- version & length check
			IPv4_OK		<= '1';
		ELSE
			IPv4_OK		<= '0';
		END IF;
		IP_SRC		<= DATA_IN(16*8-1 downto 12*8) & IP46_PREFIX;
		IP_DST 		<= DATA_IN(20*8-1 downto 16*8) & IP46_PREFIX;
		IP_PROTO 	<= DATA_IN(10*8-1 downto 9*8);
		IF (DATA_IN(10*8-1 downto 9*8) = IP_PROTO_UDP) THEN
			Is_UDP 		<= '1';
		ELSE
			Is_UDP 		<= '0';
		END IF;
		IF (DATA_IN(10*8-1 downto 9*8) = IP_PROTO_TCP) THEN
			Is_TCP 		<= '1';
		ELSE
			Is_TCP 		<= '0';
		END IF;
	WHEN "10" =>
		IF (M64_IN >= IP6_HEADER_LEN AND DATA_IN(1*8-1 downto 4) = X"6") THEN -- version & length check
			IPv6_OK		<= '1';
		ELSE
			IPv6_OK		<= '0';
		END IF;
		IP_SRC		<= DATA_IN((16+8)*8-1 downto 8*8);
		IP_DST 		<= DATA_IN((24+16)*8-1 downto 24*8);
		IP_PROTO 	<= DATA_IN(7*8-1 downto 6*8);
		IF (DATA_IN(7*8-1 downto 6*8) = IP_PROTO_UDP) THEN
			Is_UDP 		<= '1';
		ELSE
			Is_UDP 		<= '0';
		END IF;
		IF (DATA_IN(7*8-1 downto 6*8) = IP_PROTO_TCP) THEN
			Is_TCP 		<= '1';
		ELSE
			Is_TCP 		<= '0';
		END IF;
	WHEN OTHERS =>
		NULL;
END CASE;
ELSE
	IPv4_OK		<= '0';
	IPv6_OK		<= '0';
END IF;
--
Av_OUT 		<= Av;
--En_OUT      <= IPv4_OK AND (Is_UDP OR Is_TCP);
UDP_Av_OUT 	<= Is_UDP;
TCP_Av_OUT 	<= Is_TCP;
IP_SRC_OUT	<= IP_SRC;
IP_DST_OUT	<= IP_DST;
PROTO_OUT	<= IP_PROTO;
M64_Decr4	<= M64_IN - IP4_HEADER_LEN;
M64_Decr6	<= M64_IN - IP6_HEADER_LEN;
IF (IPv4_OK = '1') THEN
	En_OUT 		<= '1';
	M64_OUT		<= M64_Decr4;
	DATA_OUT(2*512-20*8-1 downto 0) 	<= DATA(2*512-1 downto 20*8 );
ELSIF (IPv6_OK = '1') THEN
	En_OUT 		<= '1';
	M64_OUT		<= M64_Decr6;
	DATA_OUT(2*512-40*8-1 downto 0) 	<= DATA(2*512-1 downto 40*8 );
ELSE
	En_OUT 		<= '0';
	M64_OUT		<= M64;
	DATA_OUT 	<= DATA;
END IF;
--
END IF;
END PROCESS;

END STRUCTURE;

--
---- L4
--

library IEEE;
use IEEE.std_logic_1164.all;
USE IEEE.std_logic_UNSIGNED.ALL;
USE work.tuw.all;
--
ENTITY DEC_L4 IS
        PORT (
             Clk 		: IN   STD_LOGIC;       -- Clock
--
             DATA_IN 	: IN   STD_LOGIC_VECTOR(2*512-1 downto 0); -- Data In
             AV_IN 		: IN   STD_LOGIC;
             En_UDP_IN 	: IN   STD_LOGIC;   
             En_TCP_IN 	: IN   STD_LOGIC;   
             M64_IN 	: IN   STD_LOGIC_VECTOR(7-1 downto 0);
--             
             DATA_OUT	: OUT  STD_LOGIC_VECTOR(2*512-1 downto 0);
             AV_OUT 	: OUT  STD_LOGIC;
             En_OUT     : OUT  STD_LOGIC;
             M64_OUT 	: OUT  STD_LOGIC_VECTOR(7-1 downto 0);
             L4_SRC_OUT : OUT  STD_LOGIC_VECTOR(2*8-1 downto 0);
             L4_DST_OUT : OUT  STD_LOGIC_VECTOR(2*8-1 downto 0);
             UDP_LEN_OUT  : OUT  STD_LOGIC_VECTOR(2*8-1 downto 0);
             UDP_DATA_OUT : OUT  STD_LOGIC_VECTOR(128-1 downto 0);
             UDP_Av_OUT : OUT  STD_LOGIC;
             TCP_Av_OUT : OUT  STD_LOGIC
             );
END DEC_L4;

ARCHITECTURE STRUCTURE OF DEC_L4 IS 
--
SIGNAL Av						: STD_LOGIC;
SIGNAL M64 					: STD_LOGIC_VECTOR(7-1 downto 0);
SIGNAL DATA 				: STD_LOGIC_VECTOR(2*512-1 downto 0);
SIGNAL En_TCP				: STD_LOGIC;
SIGNAL En_UDP				: STD_LOGIC;
SIGNAL L4_SRC				: STD_LOGIC_VECTOR(16-1 downto 0);
SIGNAL L4_DST				: STD_LOGIC_VECTOR(16-1 downto 0);
SIGNAL L4_OK				: STD_LOGIC;

CONSTANT L4_HEADER_LEN      : STD_LOGIC_VECTOR(7-1 downto 0) := "0000010";
--
BEGIN

-- + 2D
UShd: PROCESS(clk)
BEGIN
IF (clk'event AND clk = '1') THEN
Av		<= Av_IN;
M64		<= M64_IN;
DATA 	<= DATA_IN;
En_UDP 	<= En_UDP_IN;
En_TCP 	<= En_TCP_IN;
-- IPv4
IF (Av_IN = '1' AND (En_UDP_IN = '1' OR En_TCP_IN = '1')) THEN
	L4_SRC	<= DATA_IN(2*8-1 downto 0);
	L4_DST	<= DATA_IN(4*8-1 downto 2*8);
	IF (M64_IN < L4_HEADER_LEN) THEN 
		L4_OK	<= '0';
	ELSE
		L4_OK 	<= '1';
	END IF;
ELSE
	L4_OK 		<= '0';
END IF;
--
Av_OUT 			<= Av;
En_OUT      <= L4_OK AND (En_TCP OR En_UDP);
UDP_Av_OUT 	<= L4_OK AND En_UDP;
TCP_Av_OUT 	<= L4_OK AND En_TCP;
L4_SRC_OUT  <= L4_SRC;
L4_DST_OUT  <= L4_DST;
--
END IF;
END PROCESS;

END STRUCTURE;