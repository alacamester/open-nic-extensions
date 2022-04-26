library IEEE;
use IEEE.std_logic_1164.all;
use work.tuw.all;
--      ==========
PACKAGE pkg_MON100G IS
--      ==========
COMPONENT Monitor_100G_simple
        PORT (
             Clk 			: IN  STD_LOGIC;       -- Clock
             clk_pci        : IN  STD_LOGIC;             
             Rst 			: IN  STD_LOGIC;
-- Filter data Wr
             DATA_IN 		: IN  STD_LOGIC_VECTOR(32-1 downto 0); -- Data In
             DATA_Wr 		: IN  STD_LOGIC;      
-- CMAC AXI IN
			 CMAC_Dav_IN 	: IN  STD_LOGIC;
			 CMAC_Data_IN	: IN  STD_LOGIC_VECTOR(512-1 downto 0);
			 CMAC_Tkeep_IN 	: IN  STD_LOGIC_VECTOR(64-1 downto 0); -- byte enable
			 CMAC_Eof_IN	: IN  STD_LOGIC;
-- CMAC AXI OUT
			CMAC_Dav_OUT 	: OUT STD_LOGIC;
			CMAC_Data_OUT 	: OUT STD_LOGIC_VECTOR(512-1 downto 0);
			CMAC_Tkeep_OUT 	: OUT STD_LOGIC_VECTOR(64-1 downto 0);
			CMAC_Eof_OUT 	: OUT STD_LOGIC
		   );
END COMPONENT;

COMPONENT lane_FIFO
	GENERIC( 
	AEmpty_Offset : integer := 48; 
	AFull_Offset : integer := 64
	);
	PORT(
	Rst			: IN  STD_LOGIC;
	WClk			: IN  STD_LOGIC;
	We			: IN  STD_LOGIC;
	WrAv			: OUT STD_LOGIC;
	WDin			: IN  STD_LOGIC_VECTOR(64+8-1 downto 0);
	--
	RClk			: IN  STD_LOGIC;
	Re			: IN  STD_LOGIC;
	RdAv			: OUT STD_LOGIC;
	RDout		: OUT STD_LOGIC_VECTOR(64+8-1 downto 0)
	);
END COMPONENT;

END pkg_MON100G;

library IEEE;
use IEEE.std_logic_1164.all;
USE IEEE.std_logic_UNSIGNED.ALL;
USE work.tuw.all;
USE work.pkg_MON100G.all;
USE work.pkg_DEC100G.all;
USE work.pkg_FLT100G.all;
USE work.FunctionsPkg_base.all;
--
Library xpm;
use xpm.vcomponents.all;

ENTITY Monitor_100G_simple IS
        PORT (
             Clk 			: IN  STD_LOGIC;       -- Clock
             clk_pci        : IN  STD_LOGIC;
             Rst 			: IN  STD_LOGIC;
-- Filter data Wr
             DATA_IN 		: IN  STD_LOGIC_VECTOR(32-1 downto 0); -- Data In
             DATA_Wr 		: IN  STD_LOGIC;      
-- CMAC AXI IN
			 CMAC_Dav_IN 	: IN  STD_LOGIC;
			 CMAC_Data_IN	: IN  STD_LOGIC_VECTOR(512-1 downto 0);
			 CMAC_Tkeep_IN 	: IN  STD_LOGIC_VECTOR(64-1 downto 0); -- byte enable
			 CMAC_Eof_IN	: IN  STD_LOGIC;
-- CMAC AXI OUT
			CMAC_Dav_OUT 	: OUT STD_LOGIC;
			CMAC_Data_OUT 	: OUT STD_LOGIC_VECTOR(512-1 downto 0);
			CMAC_Tkeep_OUT 	: OUT STD_LOGIC_VECTOR(64-1 downto 0);
			CMAC_Eof_OUT 	: OUT STD_LOGIC
             );
END Monitor_100G_simple;

ARCHITECTURE STRUCTURE OF Monitor_100G_simple IS 
--
SIGNAL DATA 		: STD_LOGIC_VECTOR(512-1 downto 0);
SIGNAL TKEEP 		: STD_LOGIC_VECTOR(64-1 downto 0);
SIGNAL M64 			: STD_LOGIC_VECTOR(6-1 downto 0);
SIGNAL SOF 			: STD_LOGIC;
SIGNAL EOF 			: STD_LOGIC;
SIGNAL DAV 			: STD_LOGIC;
SIGNAL nCMAC_Tkeep  : STD_LOGIC_VECTOR(64-1 downto 0);

TYPE tyFS_State     IS (FS_IDLE, FS_MORE);
SIGNAL FS_State     : tyFS_State;

SIGNAL IP_Av      : STD_LOGIC;
SIGNAL IP_SRC     : STD_LOGIC_VECTOR(16*8-1 downto 0);
SIGNAL IP_DST     : STD_LOGIC_VECTOR(16*8-1 downto 0);
SIGNAL IP_PROTO   : STD_LOGIC_VECTOR(8-1 downto 0);

SIGNAL L4_Av        : STD_LOGIC;
SIGNAL L4_UDP_Av    : STD_LOGIC;
SIGNAL L4_TCP_Av    : STD_LOGIC;
SIGNAL L4_SRC       : STD_LOGIC_VECTOR(16-1 downto 0);
SIGNAL L4_DST       : STD_LOGIC_VECTOR(16-1 downto 0);

SIGNAL Decode_Done  : STD_LOGIC;

SIGNAL CFLT_DATA 	: STD_LOGIC_VECTOR(512-1 downto 0);
SIGNAL CFLT_TKEEP 	: STD_LOGIC_VECTOR(64-1 downto 0);
SIGNAL CFLT_SOF 	: STD_LOGIC;
SIGNAL CFLT_EOF 	: STD_LOGIC;
SIGNAL CFLT_DAV 	: STD_LOGIC;

SIGNAL Rst_cmac     : STD_LOGIC;
SIGNAL Rst_r        : STD_LOGIC_VECTOR(4-1 downto 0) := (others => '0');
SIGNAL Rst_dly      : STD_LOGIC := '0';
SIGNAL FF_Wr 		: STD_LOGIC;
SIGNAL FF_Rd 		: STD_LOGIC;
SIGNAL FF_Din 		: STD_LOGIC_VECTOR(9*72-1 downto 0);
SIGNAL FF_Dout 		: STD_LOGIC_VECTOR(9*72-1 downto 0);

SIGNAL Filter_Done 	: STD_LOGIC;
SIGNAL Filter_Match	: STD_LOGIC;
SIGNAL Filter_Match_r: STD_LOGIC;
SIGNAL Filter_passed: STD_LOGIC;
--
BEGIN

nCMAC_Tkeep     <= NOT CMAC_Tkeep_IN;

xpm_cdc_single_inst : xpm_cdc_single
   generic map (
      DEST_SYNC_FF => 4,   -- DECIMAL; range: 2-10
      INIT_SYNC_FF => 0,   -- DECIMAL; 0=disable simulation init values, 1=enable simulation init values
      SIM_ASSERT_CHK => 0, -- DECIMAL; 0=disable simulation messages, 1=enable simulation messages
      SRC_INPUT_REG => 1   -- DECIMAL; 0=do not register input, 1=register input
   )
   port map (
      dest_out => Rst_cmac, -- 1-bit output: src_in synchronized to the destination clock domain. This output
                            -- is registered.

      dest_clk => clk, -- 1-bit input: Clock signal for the destination clock domain.
      src_clk => clk_pci,   -- 1-bit input: optional; required when SRC_INPUT_REG = 1
      src_in => Rst      -- 1-bit input: Input signal to be synchronized to dest_clk domain.
   );


UA2M: PROCESS(clk)
BEGIN
IF (clk'event AND clk = '1') THEN
	M64 	<= conv_stdlogic(seekthe1_left(nCMAC_Tkeep), 6);
	TKEEP 	<= CMAC_Tkeep_IN;
	DATA 	<= CMAC_Data_IN;
	DAV 	<= CMAC_Dav_IN;
	EOF 	<= CMAC_Eof_IN;
IF (Rst_dly = '1') THEN
	FS_State 	<= FS_IDLE;
	SOF 		<= '0';
ELSE
	CASE FS_State IS
		WHEN FS_IDLE =>
			IF (CMAC_Dav_IN = '1') THEN
				SOF 	<= '1';
				IF (CMAC_Eof_IN = '0') THEN
					FS_State 	<= FS_MORE;
				END IF;
			ELSE
				SOF 	<= '0';
			END IF;
		WHEN FS_MORE =>
		    SOF       <= '0';
			IF (CMAC_Dav_IN = '1' AND CMAC_Eof_IN = '1') THEN
				FS_State 	<= FS_IDLE;
			END IF;
		WHEN OTHERS =>
			NULL;
	END CASE;
END IF;
END IF;
END PROCESS;


 UDEC: DEC_100G_simple
        PORT MAP (
             Clk 		=> clk, --: IN   STD_LOGIC;       -- Clock
             Rst        => Rst_dly,
--
             DATA_IN 	=> DATA, --: IN   STD_LOGIC_VECTOR(512-1 downto 0); -- Data In
             SOF_IN 	=> SOF, --: IN   STD_LOGIC;      
             DAV_IN 	=> DAV, --: IN   STD_LOGIC;
             EOF_IN 	=> EOF, --: IN   STD_LOGIC;      
             M64_IN 	=> M64, --: IN   STD_LOGIC_VECTOR(6-1 downto 0);
--             
             IP_Av_OUT		=> IP_Av, --: OUT  STD_LOGIC_VECTOR(2*512-1 downto 0);
             IP_SRC_OUT 	=> IP_SRC, --: OUT  STD_LOGIC_VECTOR(32-1 downto 0);
             IP_DST_OUT 	=> IP_DST, --: OUT  STD_LOGIC_VECTOR(32-1 downto 0);
             IP_PROTO_OUT 	=> IP_PROTO,
             L4_Av_OUT      => L4_Av,
             L4_UDP_Av_OUT 	=> L4_UDP_Av, --: OUT  STD_LOGIC;
             L4_TCP_Av_OUT 	=> L4_TCP_Av, --: OUT  STD_LOGIC;
             L4_SRC_OUT		=> L4_SRC, --: OUT  STD_LOGIC_VECTOR(16-1 downto 0);
             L4_DST_OUT		=> L4_DST, --: OUT  STD_LOGIC_VECTOR(16-1 downto 0);
             Decode_Done_OUT => Decode_Done --: OUT  STD_LOGIC
             );

UFLT: Filter_100G_simple
        PORT MAP (
             Clk            => clk,
             clk_pci        => clk_pci,
             Rst            => Rst,
             DATA_IN 		=> DATA_IN, --: IN  STD_LOGIC_VECTOR(32-1 downto 0); -- Data In
             DAV_IN 		=> DATA_Wr, --: IN  STD_LOGIC;      
--             
			 Decode_Done_IN => Decode_Done, --: IN  STD_LOGIC;
			 IP_Av_IN  		=> IP_Av, --: IN  STD_LOGIC;
			 IP_SRC_IN 		=> IP_SRC, --: IN  STD_LOGIC_VECTOR(32-1 downto 0);
			 IP_DST_IN 		=> IP_DST, --: IN  STD_LOGIC_VECTOR(32-1 downto 0);
			 IP_PROTO_IN 	=> IP_PROTO, --: IN  STD_LOGIC_VECTOR(8-1 downto 0);
			 L4_SRC_IN 	   	=> L4_SRC, --: IN  STD_LOGIC_VECTOR(16-1 downto 0);
			 L4_DST_IN 	   	=> L4_DST, --: IN  STD_LOGIC_VECTOR(16-1 downto 0);
			 L4_Av_IN  	    => L4_Av, --: IN  STD_LOGIC;
			 --
			 Filter_Done_OUT  => Filter_Done, --: OUT STD_LOGIC;
			 Filter_Match_OUT => Filter_Match --: OUT STD_LOGIC
        ); 

--FF_Din 	<= "000000" & CMAC_Eof_IN & CMAC_Dav_IN & CMAC_Tkeep_IN & CMAC_Data_IN; -- 512 + 64 + 2
UFFCtl: PROCESS(clk)
BEGIN
IF (clk'event AND clk = '1') THEN
Rst_r   <= Rst_r(Rst_r'length-2 downto 0) & Rst_cmac;
Rst_dly   <= Rst_r(Rst_r'length-1);
FF_Din 	<= X"0000000000000000" & "00000" & DAV & EOF & SOF & TKEEP & DATA; -- 512 + 64 + 3

	IF (Rst_dly = '1') THEN
		FF_Wr 	<= '0';
	ELSIF (SOF = '1') THEN
		FF_Wr 	<= '1';
	END IF;
	-- 
	IF (Rst_dly = '1') THEN
		FF_Rd 	<= '0';
	ELSIF (Filter_Done = '1') THEN -- 1st starts readout
		FF_Rd 	<= '1';
	END IF;
END IF;
END PROCESS;

UMd: FOR F IN 0 TO (9-1) GENERATE
BEGIN
Ug: lane_FIFO
	PORT MAP(
	Rst			=> Rst_cmac, --: IN  STD_LOGIC;
	WClk		=> clk, --: IN  STD_LOGIC;
	We			=> FF_Wr,
	WrAv		=> open, --: OUT STD_LOGIC;
	WDin		=> FF_Din(72*(F+1)-1 downto 72*F), --: IN  STD_LOGIC_VECTOR(66-1 downto 0);
	--
	RClk		=> clk, --	: IN  STD_LOGIC;
	Re			=> FF_Rd, --: IN  STD_LOGIC;
	RdAv		=> open, --: OUT STD_LOGIC;
	RDout		=> FF_Dout(72*(F+1)-1 downto 72*F) --: OUT STD_LOGIC_VECTOR(66-1 downto 0)
	);

END GENERATE;

CFLT_Data 	<= FF_Dout(512-1 downto 0);
CFLT_Tkeep 	<= FF_Dout(512+64-1 downto 512);
CFLT_SOF 	<= FF_Dout(512+64);
CFLT_EOF 	<= FF_Dout(512+64+1);
CFLT_DAV 	<= FF_Dout(512+64+2);


UOut: PROCESS(clk)
BEGIN
IF (clk'event AND clk = '1') THEN
CMAC_Data_OUT 	<= CFLT_Data;
CMAC_Tkeep_OUT 	<= CFLT_Tkeep;
Filter_Match_r 	<= Filter_Match;
IF (CFLT_SOF = '1') THEN
	IF (Filter_Match_r = '1') THEN
		CMAC_Dav_OUT 	<= CFLT_DAV;
		CMAC_Eof_OUT 	<= CFLT_EOF;
		Filter_passed	<= '1';
	ELSE
		CMAC_Dav_OUT 	<= '0';
		CMAC_Eof_OUT 	<= '0';
		Filter_passed	<= '0';
	END IF;
ELSE
	CMAC_Dav_OUT 	<= CFLT_DAV AND Filter_passed;
	CMAC_Eof_OUT 	<= CFLT_EOF AND Filter_passed;
END IF;
END IF;
END PROCESS;

END STRUCTURE;

library IEEE;
use IEEE.std_logic_1164.all;
USE IEEE.std_logic_UNSIGNED.ALL;
USE IEEE.numeric_std.all;
use work.tuw.all;

library UNISIM;
use UNISIM.VCOMPONENTS.ALL; 

ENTITY lane_FIFO IS
	GENERIC( 
	AEmpty_Offset : integer := 48; 
	AFull_Offset : integer := 64
	);
	PORT(
	Rst			: IN  STD_LOGIC;
	WClk			: IN  STD_LOGIC;
	We			: IN  STD_LOGIC;
	WrAv			: OUT STD_LOGIC;
	WDin			: IN  STD_LOGIC_VECTOR(64+8-1 downto 0);
	--
	RClk			: IN  STD_LOGIC;
	Re			: IN  STD_LOGIC;
	RdAv			: OUT STD_LOGIC;
	RDout		: OUT STD_LOGIC_VECTOR(64+8-1 downto 0)
	);
END lane_FIFO;

ARCHITECTURE lane_FIFO_arch OF lane_FIFO IS

SIGNAL REmpty		: STD_LOGIC;
SIGNAL RAEmpty		: STD_LOGIC;
SIGNAL RAFull		: STD_LOGIC;
SIGNAL WDIP		: STD_LOGIC_VECTOR(8-1 downto 0);
SIGNAL RDOP		: STD_LOGIC_VECTOR(8-1 downto 0);

SIGNAL RDRSTBUSY : STD_LOGIC;
SIGNAL WRRSTBUSY : STD_LOGIC;

BEGIN

--RdAv		<= NOT REmpty;
RdAv		<= NOT RAEmpty;
WrAv		<= NOT RAFull;

RDout(64+8-1 downto 64)	<= RDOP(8-1 downto 0);

WDIP		<= WDin(64+8-1 downto 64);


--UFF : FIFO36_72
--GENERIC MAP(
--	ALMOST_FULL_OFFSET		=> AFull_Offset, --X"0040", -- Sets almost full threshold: 4096/8=0x200 Full; (4096-1540)/8 AFull
--	ALMOST_EMPTY_OFFSET		=> AEmpty_Offset, --X"0010", -- Sets the almost empty threshold
--	DO_REG				=> 1, -- Enable output register (0 or 1)
--	-- Must be 1 if EN_SYN = FALSE
--	EN_ECC_READ			=> FALSE, -- Enable ECC decoder, TRUE or FALSE
--	EN_ECC_WRITE			=> FALSE, -- Enable ECC encoder, TRUE or FALSE
--	EN_SYN				=> FALSE, -- Specifies FIFO as Asynchronous (FALSE)
--	-- or Synchronous (TRUE)
--	FIRST_WORD_FALL_THROUGH	=> TRUE 
--	) -- Sets the FIFO FWFT to TRUE or FALSE
--port map (
--	RST			=> Rst, -- 1-bit reset input
---- Wr
--	WRCLK		=> WClk, -- 1-bit write clock input
--	WREN			=> We, -- 1-bit write enable input
--	DI			=> WDin(64-1 downto 0), -- 64-bit data input
--	DIP			=> WDIP, -- 8-bit parity input
--	WRCOUNT		=> open, -- 9-bit write count output
--	WRERR		=> open, -- 1-bit write error
---- Rd
--	RDCLK		=> RClk, -- 1-bit read clock input
--	RDEN			=> Re, -- 1-bit read enable input
--	DO			=> RDout(64-1 downto 0), -- 64-bit data output
--	DOP			=> RDOP, -- 8-bit parity data output
--	RDCOUNT		=> open, -- 9-bit read count output
--	RDERR		=> open, -- 1-bit read error output
---- Stat
--	EMPTY		=> REmpty, -- 1-bit empty output flag
--	FULL			=> open, -- 1-bit full output flag
--	ALMOSTEMPTY	=> RAEmpty, -- 1-bit almost empty output flag
--	ALMOSTFULL	=> RAFull, -- 1-bit almost full output flag
--	DBITERR		=> open, -- 1-bit double bit error status output
--	ECCPARITY		=> open -- 8-bit generated error correction parity
--	);

   UFF : FIFO36E2
   generic map (
      CASCADE_ORDER => "NONE",            -- FIRST, LAST, MIDDLE, NONE, PARALLEL
      CLOCK_DOMAINS => "INDEPENDENT",     -- COMMON, INDEPENDENT
      EN_ECC_PIPE => "FALSE",             -- ECC pipeline register, (FALSE, TRUE)
      EN_ECC_READ => "FALSE",             -- Enable ECC decoder, (FALSE, TRUE)
      EN_ECC_WRITE => "FALSE",            -- Enable ECC encoder, (FALSE, TRUE)
      FIRST_WORD_FALL_THROUGH => "TRUE", -- FALSE, TRUE
      INIT => X"000000000000000000",      -- Initial values on output port
      PROG_EMPTY_THRESH => AEmpty_Offset,           -- Programmable Empty Threshold
      PROG_FULL_THRESH => 512-AFull_Offset,            -- Programmable Full Threshold
      -- Programmable Inversion Attributes: Specifies the use of the built-in programmable inversion
      IS_RDCLK_INVERTED => '0',           -- Optional inversion for RDCLK
      IS_RDEN_INVERTED => '0',            -- Optional inversion for RDEN
      IS_RSTREG_INVERTED => '0',          -- Optional inversion for RSTREG
      IS_RST_INVERTED => '0',             -- Optional inversion for RST
      IS_WRCLK_INVERTED => '0',           -- Optional inversion for WRCLK
      IS_WREN_INVERTED => '0',            -- Optional inversion for WREN
      RDCOUNT_TYPE => "RAW_PNTR",         -- EXTENDED_DATACOUNT, RAW_PNTR, SIMPLE_DATACOUNT, SYNC_PNTR
      READ_WIDTH => 72,                    -- 18-9
      REGISTER_MODE => "REGISTERED",    -- DO_PIPELINED, REGISTERED, UNREGISTERED
      RSTREG_PRIORITY => "RSTREG",        -- REGCE, RSTREG
      SLEEP_ASYNC => "FALSE",             -- FALSE, TRUE
      SRVAL => X"000000000000000000",     -- SET/reset value of the FIFO outputs
      WRCOUNT_TYPE => "RAW_PNTR",         -- EXTENDED_DATACOUNT, RAW_PNTR, SIMPLE_DATACOUNT, SYNC_PNTR
      WRITE_WIDTH => 72                    -- 18-9
   )
   port map (
      -- Cascade Signals outputs: Multi-FIFO cascade signals
      CASDOUT => open,             -- 64-bit output: Data cascade output bus
      CASDOUTP => open,           -- 8-bit output: Parity data cascade output bus
      CASNXTEMPTY => open,     -- 1-bit output: Cascade next empty
      CASPRVRDEN => open,       -- 1-bit output: Cascade previous read enable
      -- ECC Signals outputs: Error Correction Circuitry ports
      DBITERR => open,             -- 1-bit output: Double bit error status
      ECCPARITY => open,         -- 8-bit output: Generated error correction parity
      SBITERR => open,             -- 1-bit output: Single bit error status
      -- Read Data outputs: Read output data
      DOUT => RDout(64-1 downto 0),                   -- 64-bit output: FIFO data output bus
      DOUTP => RDOP,                 -- 8-bit output: FIFO parity output bus.
      -- Status outputs: Flags and other FIFO status outputs
      EMPTY => REmpty,                 -- 1-bit output: Empty
      FULL => open,                   -- 1-bit output: Full
      PROGEMPTY => RAEmpty,         -- 1-bit output: Programmable empty
      PROGFULL => RAFull,           -- 1-bit output: Programmable full
      RDCOUNT => open,             -- 14-bit output: Read count
      RDERR => open,                 -- 1-bit output: Read error
      RDRSTBUSY => RDRSTBUSY,         -- 1-bit output: Reset busy (sync to RDCLK)
      WRCOUNT => open,             -- 14-bit output: Write count
      WRERR => open,                 -- 1-bit output: Write Error
      WRRSTBUSY => WRRSTBUSY,         -- 1-bit output: Reset busy (sync to WRCLK)
      -- Cascade Signals inputs: Multi-FIFO cascade signals
      CASDIN => (others => '0'),               -- 64-bit input: Data cascade input bus
      CASDINP => (others => '0'),             -- 8-bit input: Parity data cascade input bus
      CASDOMUX => '0',           -- 1-bit input: Cascade MUX select input
      CASDOMUXEN => '0',       -- 1-bit input: Enable for cascade MUX select
      CASNXTRDEN => '0',       -- 1-bit input: Cascade next read enable
      CASOREGIMUX => '0',     -- 1-bit input: Cascade output MUX select
      CASOREGIMUXEN => '0', -- 1-bit input: Cascade output MUX select enable
      CASPRVEMPTY => '0',     -- 1-bit input: Cascade previous empty
      -- ECC Signals inputs: Error Correction Circuitry ports
      INJECTDBITERR => '0', -- 1-bit input: Inject a double bit error
      INJECTSBITERR => '0', -- 1-bit input: Inject a single bit error
      -- Read Control Signals inputs: Read clock, enable and reset input signals
      RDCLK => RClk,                 -- 1-bit input: Read clock
      RDEN => Re,                   -- 1-bit input: Read enable
      REGCE => '1',                 -- 1-bit input: Output register clock enable
      RSTREG => '0',               -- 1-bit input: Output register reset
      SLEEP => '0',                 -- 1-bit input: Sleep Mode
      -- Write Control Signals inputs: Write clock and enable input signals
      RST => Rst,                     -- 1-bit input: Reset
      WRCLK => WClk,                 -- 1-bit input: Write clock
      WREN => We,                   -- 1-bit input: Write enable
      -- Write Data inputs: Write input data
      DIN => WDin(64-1 downto 0),                     -- 64-bit input: FIFO data input bus
      DINP => WDIP                    -- 8-bit input: FIFO parity input bus
   );

END lane_FIFO_arch;

