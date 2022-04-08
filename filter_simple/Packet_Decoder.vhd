library IEEE;
use IEEE.std_logic_1164.all;
use work.tuw.all;
--      ==========
PACKAGE pkg_DEC100G IS
--      ==========
  COMPONENT aProgPipe
--          ~~~~~~~~~
        GENERIC( Size:   INTEGER := 32 );
        PORT (
             I:     IN     STD_LOGIC_VECTOR (Size-1 downto 0); -- Input
             C:     IN     STD_LOGIC;                          -- Clock
             CE:    IN     STD_LOGIC;                          -- Enable
             A:     IN     STD_LOGIC_VECTOR (3 downto 0);      -- Addr
             Q:     OUT    STD_LOGIC_VECTOR (Size-1 downto 0)  -- Output
             );
  END COMPONENT;

COMPONENT aProgPipe_1
--          ~~~~~~~~~
PORT (
	I 				: IN  STD_LOGIC; -- Input
	C 				: IN  STD_LOGIC;                          -- Clock
	CE 				: IN  STD_LOGIC;                          -- Enable
	A				: IN  STD_LOGIC_VECTOR (3 downto 0);      -- Addr
	Q				: OUT STD_LOGIC  -- Output
	);
END COMPONENT; 

  COMPONENT DEC_100G_simple
        PORT (
             Clk 		: IN   STD_LOGIC;       -- Clock
             Rst        : IN   STD_LOGIC;             
--
             DATA_IN 	: IN   STD_LOGIC_VECTOR(512-1 downto 0); -- Data In
             SOF_IN 	: IN   STD_LOGIC;      
             DAV_IN     : IN   STD_LOGIC;
             EOF_IN 	: IN   STD_LOGIC;      
             M64_IN 	: IN   STD_LOGIC_VECTOR(6-1 downto 0);
--             
             IPv4_Av_OUT	: OUT  STD_LOGIC;
             IPv4_SRC_OUT 	: OUT  STD_LOGIC_VECTOR(32-1 downto 0);
             IPv4_DST_OUT 	: OUT  STD_LOGIC_VECTOR(32-1 downto 0);
             IPv4_PROTO_OUT : OUT  STD_LOGIC_VECTOR(8-1 downto 0);             
             L4_Av_OUT      : OUT  STD_LOGIC;             
             L4_UDP_Av_OUT 	: OUT  STD_LOGIC;
             L4_TCP_Av_OUT 	: OUT  STD_LOGIC;
             L4_SRC_OUT		: OUT  STD_LOGIC_VECTOR(16-1 downto 0);
             L4_DST_OUT		: OUT  STD_LOGIC_VECTOR(16-1 downto 0);
             Decode_Done_OUT : OUT  STD_LOGIC
             );
  END COMPONENT;

END PACKAGE;

library IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.std_logic_UNSIGNED.ALL;
USE work.tuw.ALL;
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;
--
ENTITY      aProgPipe   IS
--          ~~~~~~~~~
        GENERIC( Size:   INTEGER := 32 );
        PORT (
             I:     IN     STD_LOGIC_VECTOR (Size-1 downto 0);  -- In
             C:     IN     STD_LOGIC;         -- Clock
             CE:    IN     STD_LOGIC;         -- Clock Enable
             A:     IN     STD_LOGIC_VECTOR (3 downto 0);       -- Addr.
             Q:     OUT    STD_LOGIC_VECTOR (Size-1 downto 0)); -- Out
END         aProgPipe;
--
ARCHITECTURE LUTCellPPP  OF aProgPipe    IS
--           ~~~~~~~~~~~~~~~~~~~~~~~~~~
BEGIN
--
G0: FOR J IN 0 TO Size-1 GENERATE
U0: SRL16E 
GENERIC MAP (
	INIT =>x"0000" )
PORT MAP (
	D		=>I(J), 
	CE		=>CE, 
	CLK		=>C,
	A0		=>A(0),
	A1		=>A(1),
	A2		=>A(2),
	A3		=>A(3),
	Q		=>Q(J) 
	  );
END GENERATE;
--
END  LUTCellPPP;


library IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.std_logic_UNSIGNED.ALL;
USE work.tuw.ALL;

library UNISIM;
use UNISIM.VCOMPONENTS.ALL; 

--
ENTITY      aProgPipe_1   IS
--          ~~~~~~~~~
        PORT (
             I:     IN     STD_LOGIC;  -- In
             C:     IN     STD_LOGIC;         -- Clock
             CE:    IN     STD_LOGIC;         -- Clock Enable
             A:     IN     STD_LOGIC_VECTOR (3 downto 0);       -- Addr.
             Q:     OUT    STD_LOGIC
             );
END aProgPipe_1;
--
ARCHITECTURE LUTCellPPP  OF aProgPipe_1 IS
--           ~~~~~~~~~~~~~~~~~~~~~~~~~~
BEGIN
--
U0: SRL16E PORT MAP (
       D=>I, CE=>CE, CLK=>C,
       A0=>A(0),A1=>A(1),A2=>A(2),A3=>A(3),
       Q=>Q );
 --
END  LUTCellPPP; 

library IEEE;
use IEEE.std_logic_1164.all;
USE IEEE.std_logic_UNSIGNED.ALL;
USE work.tuw.all;
USE work.pkg_DEC100G.all;
USE work.pkg_DECmod100G.all;
--
ENTITY DEC_100G_simple IS
        PORT (
             Clk 		: IN   STD_LOGIC;       -- Clock
             Rst        : IN   STD_LOGIC;
--
             DATA_IN 	: IN   STD_LOGIC_VECTOR(512-1 downto 0); -- Data In
             SOF_IN 	: IN   STD_LOGIC;      
             DAV_IN     : IN   STD_LOGIC;
             EOF_IN 	: IN   STD_LOGIC;      
             M64_IN 	: IN   STD_LOGIC_VECTOR(6-1 downto 0);
--             
             IPv4_Av_OUT	: OUT  STD_LOGIC;
             IPv4_SRC_OUT 	: OUT  STD_LOGIC_VECTOR(32-1 downto 0);
             IPv4_DST_OUT 	: OUT  STD_LOGIC_VECTOR(32-1 downto 0);
             IPv4_PROTO_OUT : OUT  STD_LOGIC_VECTOR(8-1 downto 0);
             L4_Av_OUT      : OUT  STD_LOGIC;
             L4_UDP_Av_OUT 	: OUT  STD_LOGIC;
             L4_TCP_Av_OUT 	: OUT  STD_LOGIC;
             L4_SRC_OUT		: OUT  STD_LOGIC_VECTOR(16-1 downto 0);
             L4_DST_OUT		: OUT  STD_LOGIC_VECTOR(16-1 downto 0);
             Decode_Done_OUT : OUT  STD_LOGIC             
             );
END DEC_100G_simple;

ARCHITECTURE STRUCTURE OF DEC_100G_simple IS 
--
SIGNAL PROTO_READY 		: STD_LOGIC_VECTOR(2-1 downto 0);
TYPE tySHR_ADDR 		IS ARRAY (2-1 downto 0) OF STD_LOGIC_VECTOR(4-1 downto 0);
SIGNAL SHR_ADDR 		: tySHR_ADDR; 

SIGNAL D2N_DATA			: STD_LOGIC_VECTOR(2*512-1 downto 0);
SIGNAL D2N_Av 			: STD_LOGIC;
SIGNAL D2N_SOP          : STD_LOGIC;
SIGNAL D2N_M64			: STD_LOGIC_VECTOR(7-1 downto 0);

SIGNAL DETH_DATA 		: STD_LOGIC_VECTOR(2*512-1 downto 0);
SIGNAL DETH_Av 			: STD_LOGIC;
SIGNAL DETH_M64			: STD_LOGIC_VECTOR(7-1 downto 0);
SIGNAL DETH_IPv4_Av 	: STD_LOGIC;
SIGNAL DETH_IPv6_Av 	: STD_LOGIC;

SIGNAL DIP4_DATA 		: STD_LOGIC_VECTOR(2*512-1 downto 0);
SIGNAL DIP4_Av 			: STD_LOGIC;
SIGNAL DIP4_M64			: STD_LOGIC_VECTOR(7-1 downto 0);
SIGNAL DIP4_SRC 		: STD_LOGIC_VECTOR(32-1 downto 0);
SIGNAL DIP4_DST 		: STD_LOGIC_VECTOR(32-1 downto 0);
SIGNAL DIP4_PROTO		: STD_LOGIC_VECTOR(8-1 downto 0);
SIGNAL DIP4_UDP_Av 		: STD_LOGIC;
SIGNAL DIP4_TCP_Av 		: STD_LOGIC;

SIGNAL DL4_DATA 		: STD_LOGIC_VECTOR(2*512-1 downto 0);
SIGNAL DL4_Av 			: STD_LOGIC;
SIGNAL DL4_M64			: STD_LOGIC_VECTOR(7-1 downto 0);
SIGNAL DL4_SRC			: STD_LOGIC_VECTOR(16-1 downto 0);
SIGNAL DL4_DST			: STD_LOGIC_VECTOR(16-1 downto 0);
SIGNAL DL4_UDP_Av		: STD_LOGIC;
SIGNAL DL4_TCP_Av 		: STD_LOGIC;

CONSTANT DECODE_DELAY   : INTEGER := 14;

SIGNAL Decode_Done 		: STD_LOGIC;
SIGNAL D2N_SOP_r         : STD_LOGIC_VECTOR(DECODE_DELAY-1 downto 0);
--
BEGIN

Ud2n: Data2Nibble
        PORT MAP(
             Clk 		=> Clk, --: IN   STD_LOGIC;       -- Clock
--
             DATA_IN 	=> DATA_IN, --: IN   STD_LOGIC_VECTOR(512-1 downto 0); -- Data In
             SOP_IN 	=> SOF_IN, --: IN   STD_LOGIC;      
             DAV_IN     => DAV_IN, --: IN   STD_LOGIC;
             EOP_IN 	=> EOF_IN, --: IN   STD_LOGIC;      
             M64_IN 	=> M64_IN, --: IN   STD_LOGIC_VECTOR(5-1 downto 0);
--             
             DATA_OUT	=> D2N_DATA, --: OUT  STD_LOGIC_VECTOR(2*512-1 downto 0);
             AV_OUT 	=> D2N_Av, --: OUT  STD_LOGIC;
             SOP_OUT    => D2N_SOP, --: OUT  STD_LOGIC;
             M64_OUT 	=> D2N_M64 --: OUT  STD_LOGIC_VECTOR(6-1 downto 0)
             );

PROTO_READY(0)	<= DIP4_Av;
PROTO_READY(1)	<= DL4_Av;

UShOutDly: FOR SHO IN 0 TO (2-1) GENERATE
BEGIN
	Ug: PROCESS(clk, Rst, Decode_Done)
	BEGIN
	IF (Clk'event AND Clk='1') THEN
	--
	IF (Rst = '1') THEN
		RESET(SHR_ADDR(SHO));
	ELSE
		IF (PROTO_READY(SHO) = '1' AND Decode_Done = '0') THEN
			INCR(SHR_ADDR(SHO));
		ELSIF (PROTO_READY(SHO) = '0' AND Decode_Done = '1') THEN
			DECR(SHR_ADDR(SHO));
		END IF;
	END IF;
	END IF;
	END PROCESS;
END GENERATE; 

Udeth: DEC_ETH
        PORT MAP(
             Clk 		=> Clk, --: IN   STD_LOGIC;       -- Clock
--
             DATA_IN 	=> D2N_DATA, --: IN   STD_LOGIC_VECTOR(2*512-1 downto 0); -- Data In
             AV_IN 		=> D2N_Av, --: IN   STD_LOGIC;      
             M64_IN 	=> D2N_M64, --: IN   STD_LOGIC_VECTOR(6-1 downto 0);
--             
             DATA_OUT	=> DETH_DATA, --: OUT  STD_LOGIC_VECTOR(2*512-1 downto 0);
             AV_OUT 	=> DETH_Av, --: OUT  STD_LOGIC;
             M64_OUT 	=> DETH_M64, --: OUT  STD_LOGIC_VECTOR(6-1 downto 0);
             IPv4_Av_OUT=> DETH_IPv4_Av, --: OUT  STD_LOGIC;
             IPv6_Av_OUT=> DETH_IPv6_Av --: OUT  STD_LOGIC
             );

 Udip4: DEC_IPv4
        PORT MAP(
             Clk 		=> Clk, --: IN   STD_LOGIC;       -- Clock
--
             DATA_IN 	=> DETH_DATA, --: IN   STD_LOGIC_VECTOR(2*512-1 downto 0); -- Data In
             AV_IN 		=> DETH_Av, --: IN   STD_LOGIC;
             M64_IN 	=> DETH_M64, --: IN   STD_LOGIC_VECTOR(6-1 downto 0);
             En_IN 		=> DETH_IPv4_Av, --: IN   STD_LOGIC;   
--             
             DATA_OUT	=> DIP4_DATA, --: OUT  STD_LOGIC_VECTOR(2*512-1 downto 0);
             AV_OUT 	=> DIP4_Av, --: OUT  STD_LOGIC;
             M64_OUT 	=> DIP4_M64, --: OUT  STD_LOGIC_VECTOR(6-1 downto 0);
             IP_SRC_OUT => DIP4_SRC, --: OUT  STD_LOGIC_VECTOR(4*8-1 downto 0);
             IP_DST_OUT => DIP4_DST, --: OUT  STD_LOGIC_VECTOR(4*8-1 downto 0);
             PROTO_OUT  => DIP4_PROTO, --: OUT  STD_LOGIC_VECTOR(8-1 downto 0);
             UDP_Av_OUT => DIP4_UDP_Av, --: OUT  STD_LOGIC;
             TCP_Av_OUT => DIP4_TCP_Av --: OUT  STD_LOGIC
             );

Uethd_1: aProgPipe_1
	PORT MAP(C => Clk, Ce => PROTO_READY(0), A => SHR_ADDR(0), I => DIP4_Av, 	Q => IPv4_Av_OUT );
Uethd_2: aProgPipe GENERIC MAP( Size => 32 )
	PORT MAP(C => Clk, Ce => PROTO_READY(0), A => SHR_ADDR(0), I => DIP4_SRC, 	Q => IPv4_SRC_OUT );
Uethd_3: aProgPipe GENERIC MAP( Size => 32 )
	PORT MAP(C => Clk, Ce => PROTO_READY(0), A => SHR_ADDR(0), I => DIP4_DST, 	Q => IPv4_DST_OUT );
Uethd_4: aProgPipe GENERIC MAP( Size => 8 )
	PORT MAP(C => Clk, Ce => PROTO_READY(0), A => SHR_ADDR(0), I => DIP4_PROTO, 	Q => IPv4_PROTO_OUT );

Udl4: DEC_L4
        PORT MAP(
             Clk 		=> Clk, --: IN   STD_LOGIC;       -- Clock
--
             DATA_IN 	=> DIP4_DATA, --: IN   STD_LOGIC_VECTOR(2*512-1 downto 0); -- Data In
             AV_IN 		=> DIP4_Av, --: IN   STD_LOGIC;
             M64_IN 	=> DIP4_M64, --: IN   STD_LOGIC_VECTOR(6-1 downto 0);
             En_UDP_IN 	=> DIP4_UDP_Av, --: IN   STD_LOGIC;   
             En_TCP_IN 	=> DIP4_TCP_Av, --: IN   STD_LOGIC;   
--             
             DATA_OUT	=> open, --: OUT  STD_LOGIC_VECTOR(2*512-1 downto 0);
             AV_OUT 	=> DL4_Av, --: OUT  STD_LOGIC;
             M64_OUT 	=> open, --: OUT  STD_LOGIC_VECTOR(6-1 downto 0);
             L4_SRC_OUT => DL4_SRC, --: OUT  STD_LOGIC_VECTOR(4*8-1 downto 0);
             L4_DST_OUT => DL4_DST, --: OUT  STD_LOGIC_VECTOR(4*8-1 downto 0);
             UDP_LEN_OUT  => open, --: OUT  STD_LOGIC_VECTOR(2*8-1 downto 0);
             UDP_DATA_OUT => open, --: OUT  STD_LOGIC_VECTOR(128-1 downto 0);
             UDP_Av_OUT => DL4_UDP_Av, --: OUT  STD_LOGIC;
             TCP_Av_OUT => DL4_TCP_Av --: OUT  STD_LOGIC
             );


Ul4_1: aProgPipe GENERIC MAP( Size => 16 )
	PORT MAP(C => Clk, Ce => PROTO_READY(1), A => SHR_ADDR(1), I => DL4_SRC, 	Q => L4_SRC_OUT );
Ul4_2: aProgPipe GENERIC MAP( Size => 16 )
	PORT MAP(C => Clk, Ce => PROTO_READY(1), A => SHR_ADDR(1), I => DL4_DST, 	Q => L4_DST_OUT );
Ul4_3: aProgPipe_1
	PORT MAP(C => Clk, Ce => PROTO_READY(1), A => SHR_ADDR(1), I => DL4_UDP_Av, Q => L4_UDP_Av_OUT );
Ul4_4: aProgPipe_1
	PORT MAP(C => Clk, Ce => PROTO_READY(1), A => SHR_ADDR(1), I => DL4_TCP_Av, Q => L4_TCP_Av_OUT );
Ul4_5: aProgPipe_1
	PORT MAP(C => Clk, Ce => PROTO_READY(1), A => SHR_ADDR(1), I => DL4_Av, Q => L4_Av_OUT );


PROCESS(Clk)
BEGIN
IF (Clk'event AND Clk = '1') THEN
    D2N_SOP_r       <= D2N_SOP_r(D2N_SOP_r'length-2 downto 0) & D2N_SOP;
	Decode_Done 	<= D2N_SOP_r(D2N_SOP_r'length-1); --DL4_Av;
--	Decode_Done_r 	<= Decode_Done_r(Decode_Done_r'length-2 downto 0) & Decode_Done;
	Decode_Done_OUT <= Decode_Done;
END IF;
END PROCESS;

END STRUCTURE;