library IEEE;
use IEEE.std_logic_1164.all;
use work.tuw.all;
--      ==========
PACKAGE pkg_FLT100G IS
--      ==========
CONSTANT FDATA_Size : INTEGER := 312;

COMPONENT Filter_100G_simple
        PORT (
             Clk 			: IN  STD_LOGIC;       -- Clock
             clk_pci        : IN  STD_LOGIC;
             Rst 			: IN  STD_LOGIC;
--
             DATA_IN 		: IN  STD_LOGIC_VECTOR(32-1 downto 0); -- Data In
             DAV_IN 		: IN  STD_LOGIC;      
--            
             Decode_Done_IN : IN  STD_LOGIC;
			 IP_Av_IN  		: IN  STD_LOGIC;
			 IP_SRC_IN 		: IN  STD_LOGIC_VECTOR(16*8-1 downto 0);
			 IP_DST_IN 		: IN  STD_LOGIC_VECTOR(16*8-1 downto 0);
			 IP_PROTO_IN 	: IN  STD_LOGIC_VECTOR(8-1 downto 0);
			 L4_Av_IN  	    : IN  STD_LOGIC;
			 L4_SRC_IN 	   	: IN  STD_LOGIC_VECTOR(16-1 downto 0);
			 L4_DST_IN 	   	: IN  STD_LOGIC_VECTOR(16-1 downto 0);
             --
			 Filter_Done_OUT  : OUT STD_LOGIC;
			 Filter_Match_OUT : OUT STD_LOGIC
		   );
END COMPONENT;

COMPONENT aFilter_100G
		PORT (
		Clk 			: IN STD_LOGIC;
		--
		FDATA_IN 		: IN STD_LOGIC_VECTOR(FDATA_Size-1 downto 0);
		FDATA_Wr 		: IN STD_LOGIC;
		--
		Decode_Done 	: IN STD_LOGIC;
		IP_Av   		: IN  STD_LOGIC;
		IP_SRC    		: IN  STD_LOGIC_VECTOR(16*8-1 downto 0);
		IP_DST    		: IN  STD_LOGIC_VECTOR(16*8-1 downto 0);
		IP_PROTO      	: IN  STD_LOGIC_VECTOR(8-1 downto 0);
		L4_Av 			: IN STD_LOGIC;
		L4_SRC 			: IN STD_LOGIC_VECTOR(16-1 downto 0);
		L4_DST 			: IN STD_LOGIC_VECTOR(16-1 downto 0);
		--
		Filter_Done 	: OUT STD_LOGIC;
		Filter_Match	: OUT STD_LOGIC;
		Filter_Drop  	: OUT STD_LOGIC
		);
END COMPONENT;

END pkg_FLT100G;


library IEEE;
use IEEE.std_logic_1164.all;
USE IEEE.std_logic_UNSIGNED.ALL;
USE work.tuw.all;
USE work.pkg_FLT100G.all;
USE work.pkg_DECmod100G.IP46_PREFIX;
USE work.FunctionsPkg_base.all;
--
Library xpm;
use xpm.vcomponents.all;

ENTITY Filter_100G_simple IS
        PORT (
             Clk 			: IN  STD_LOGIC;       -- Clock
             clk_pci        : IN  STD_LOGIC;             
             Rst 			: IN  STD_LOGIC;
--
             DATA_IN 		: IN  STD_LOGIC_VECTOR(32-1 downto 0); -- Data In
             DAV_IN 		: IN  STD_LOGIC;      
--             
			 Decode_Done_IN : IN  STD_LOGIC;
			 IP_Av_IN  		: IN  STD_LOGIC;
			 IP_SRC_IN 		: IN  STD_LOGIC_VECTOR(16*8-1 downto 0);
			 IP_DST_IN 		: IN  STD_LOGIC_VECTOR(16*8-1 downto 0);
			 IP_PROTO_IN 	: IN  STD_LOGIC_VECTOR(8-1 downto 0);
			 L4_Av_IN  	    : IN  STD_LOGIC;
			 L4_SRC_IN 	   	: IN  STD_LOGIC_VECTOR(16-1 downto 0);
			 L4_DST_IN 	   	: IN  STD_LOGIC_VECTOR(16-1 downto 0);
			 --
			 Filter_Done_OUT  : OUT STD_LOGIC;
			 Filter_Match_OUT : OUT STD_LOGIC
             );
END Filter_100G_simple;

ARCHITECTURE STRUCTURE OF Filter_100G_simple IS 
--
SIGNAL DATA 			: STD_LOGIC_VECTOR(32-1 downto 0);
SIGNAL DAV 				: STD_LOGIC;
TYPE tyFLT_State 		IS (FLT_START, FLT_IP_SRC, FLT_IP_DST, FLT_IP_SRC_1, FLT_IP_SRC_2, FLT_IP_SRC_3, FLT_IP_SRC_4, FLT_IP_DST_1, FLT_IP_DST_2, FLT_IP_DST_3, FLT_IP_DST_4, FLT_L4_PORTS, FLT_CTRL);
SIGNAL FLT_State 		: tyFLT_State;

--CONSTANT FDATA_Size		: INTEGER := 312;

SIGNAL FDATA 			: STD_LOGIC_VECTOR(FDATA_Size-1 downto 0) := (others => '0');
SIGNAL MASKS            : STD_LOGIC_VECTOR(2*128-1 downto 0);

CONSTANT FLT_Num        : INTEGER := 8;

SIGNAL FDATA_Wr 		: STD_LOGIC_VECTOR(FLT_Num-1 downto 0) := (others => '0'); -- num filters
SIGNAL aFDATA_Wr 		: STD_LOGIC_VECTOR(FLT_Num-1 downto 0) := (others => '0'); -- num filters
-- clock conversion
SIGNAL FF_Din           : STD_LOGIC_VECTOR(FDATA_Size+FLT_Num-1 downto 0);
SIGNAL FF_Dout          : STD_LOGIC_VECTOR(FDATA_Size+FLT_Num-1 downto 0);
SIGNAL FF_Wr            : STD_LOGIC := '0';
SIGNAL FF_Rd            : STD_LOGIC;

-- filter xtension
CONSTANT FLT_MUL        : INTEGER := 32;

SIGNAL FF_Rd_r          : STD_LOGIC_VECTOR(FLT_MUL-1 downto 0);
SIGNAL Decode_Done      : STD_LOGIC_VECTOR(FLT_MUL-1 downto 0);
SIGNAL IP_Av  	     	: STD_LOGIC_VECTOR(FLT_MUL-1 downto 0);
SIGNAL IP_SRC     		: STD_LOGIC_VECTOR(FLT_MUL*16*8-1 downto 0);
SIGNAL IP_DST 	    	: STD_LOGIC_VECTOR(FLT_MUL*16*8-1 downto 0);
SIGNAL IP_PROTO   		: STD_LOGIC_VECTOR(FLT_MUL*8-1 downto 0);
SIGNAL L4_Av     	    : STD_LOGIC_VECTOR(FLT_MUL-1 downto 0);
SIGNAL L4_SRC   	   	: STD_LOGIC_VECTOR(FLT_MUL*16-1 downto 0);
SIGNAL L4_DST   	   	: STD_LOGIC_VECTOR(FLT_MUL*16-1 downto 0);

SIGNAL cFDATA_Sel       : STD_LOGIC_VECTOR(FLT_MUL*FLT_Num-1 downto 0) := (others => '0');
SIGNAL cFDATA_Wr        : STD_LOGIC_VECTOR(FLT_MUL*FLT_Num-1 downto 0);
SIGNAL cFDATA           : STD_LOGIC_VECTOR(FLT_MUL*FDATA_Size-1 downto 0);
SIGNAL Filter_Done 		: STD_LOGIC_VECTOR(FLT_MUL*FLT_Num-1 downto 0); -- num filters
SIGNAL Filter_Match 	: STD_LOGIC_VECTOR(FLT_MUL*FLT_Num-1 downto 0); -- num filters
SIGNAL Filter_Drop 		: STD_LOGIC_VECTOR(FLT_MUL*FLT_Num-1 downto 0); -- num filters

SIGNAL Filter_Done_r 	: STD_LOGIC;
SIGNAL Filter_Match_r 	: STD_LOGIC_VECTOR(FLT_MUL-1 downto 0);
SIGNAL Filter_Drop_r 	: STD_LOGIC_VECTOR(FLT_MUL-1 downto 0);

--
BEGIN

UIn: PROCESS(clk_pci)
BEGIN
IF (Clk_pci'event AND Clk_pci = '1') THEN
--
DATA 	<= DATA_IN;
DAV 	<= DAV_IN;
--
aFDATA_Wr(FLT_Num-1 downto 0) 	<= conv_exp2bin(FDATA(3-1 downto 0))(FLT_Num-1 downto 0); -- FLT Id
IF (Rst = '1') THEN
	FLT_State 	<= FLT_START;
	FF_Wr       <= '0';
ELSIF (DAV = '1') THEN
	CASE FLT_State IS
		WHEN FLT_START => -- PROTO & Id
			FDATA(8-1 downto 0) 		<= DATA(8-1 downto 0); -- FLT Id (can be xtended to 16 bits)
--			MASKS(32-1 downto 0) 		<= conv_exp2mask(DATA(16+5-1 downto 16))(32-1 downto 0);
--			MASKS(64-1 downto 32)		<= conv_exp2mask(DATA(24+5-1 downto 24))(32-1 downto 0);
			IF (DATA(32-1) = '1') THEN -- IPv6
                FLT_State 					<= FLT_IP_SRC_1; 
            ELSE
                FLT_State 					<= FLT_IP_SRC;
            END IF;
			FF_Wr                       <= '0';
		WHEN FLT_IP_SRC =>
		    FLT_State                   <= FLT_IP_DST;
		    FDATA(24+128-32-1 downto 24)<= IP46_PREFIX;
		    FDATA(24+128-1 downto 24+128-32)    <= DATA; -- IPv4 address
		WHEN FLT_IP_DST =>
		    FLT_State                   <= FLT_L4_PORTS;
		    FDATA(152+128-32-1 downto 152)<= IP46_PREFIX;
		    FDATA(152+128-1 downto 152+128-32)    <= DATA; -- IPv4 address
		WHEN FLT_IP_SRC_1 =>
			FDATA(24+32-1 downto 24)	<= DATA;
--			FDATA(120+32-1 downto 120) 	<= MASKS(32-1 downto 0);
			FLT_State 					<= FLT_IP_SRC_2;
		WHEN FLT_IP_SRC_2 =>
			FDATA(56+32-1 downto 56)	<= DATA;
			FLT_State 					<= FLT_IP_SRC_3;
		WHEN FLT_IP_SRC_3 =>
			FDATA(88+32-1 downto 88)	<= DATA;
			FLT_State 					<= FLT_IP_SRC_4;
		WHEN FLT_IP_SRC_4 =>
			FDATA(120+32-1 downto 120)	<= DATA;
			FLT_State 					<= FLT_IP_DST_1;
		WHEN FLT_IP_DST_1 =>
			FDATA(152+32-1 downto 152)	<= DATA;
			FLT_State 					<= FLT_IP_DST_2;
		WHEN FLT_IP_DST_2 =>
			FDATA(184+32-1 downto 184)	<= DATA;
			FLT_State 					<= FLT_IP_DST_3;
		WHEN FLT_IP_DST_3 =>
			FDATA(216+32-1 downto 216)	<= DATA;
			FLT_State 					<= FLT_IP_DST_4;
		WHEN FLT_IP_DST_4 =>
			FDATA(248+32-1 downto 248)	<= DATA;
			FLT_State 					<= FLT_L4_PORTS;
		WHEN FLT_L4_PORTS =>
			FDATA(280+32-1 downto 280)	<= DATA;
			FLT_State 					<= FLT_CTRL;
		WHEN FLT_CTRL =>
			FDATA(16-1 downto 8) 		<= DATA(8-1 downto 0); -- FLT ctrl
			FDATA(24-1 downto 16)		<= DATA(16-1 downto 8); -- IP Proto
			FLT_State 					<= FLT_START;
			FDATA_Wr             		<= aFDATA_Wr;
			FF_Wr                       <= '1';
		WHEN OTHERS =>
			NULL;	
	END CASE;
ELSE
    FF_Wr                       <= '0';
END IF;
END IF;
END PROCESS;

-- clock conversion...
FF_Din  <= FDATA_Wr & FDATA;

xpm_cdc_array_single_inst : xpm_cdc_array_single
   generic map (
      DEST_SYNC_FF => 4,   -- DECIMAL; range: 2-10
      INIT_SYNC_FF => 0,   -- DECIMAL; 0=disable simulation init values, 1=enable simulation init values
      SIM_ASSERT_CHK => 0, -- DECIMAL; 0=disable simulation messages, 1=enable simulation messages
      SRC_INPUT_REG => 1,  -- DECIMAL; 0=do not register input, 1=register input
      WIDTH => FDATA_Size+FLT_Num           -- DECIMAL; range: 1-1024
   )
   port map (
      dest_out => FF_Dout, -- WIDTH-bit output: src_in synchronized to the destination clock domain. This
                            -- output is registered.

      dest_clk => clk, -- 1-bit input: Clock signal for the destination clock domain.
      src_clk => clk_pci,   -- 1-bit input: optional; required when SRC_INPUT_REG = 1
      src_in => FF_Din      -- WIDTH-bit input: Input single-bit array to be synchronized to destination clock
                            -- domain. It is assumed that each bit of the array is unrelated to the others.
                            -- This is reflected in the constraints applied to this macro. To transfer a binary
                            -- value losslessly across the two clock domains, use the XPM_CDC_GRAY macro
                            -- instead.

   );

   xpm_cdc_pulse_inst : xpm_cdc_pulse
   generic map (
      DEST_SYNC_FF => 4,   -- DECIMAL; range: 2-10
      INIT_SYNC_FF => 0,   -- DECIMAL; 0=disable simulation init values, 1=enable simulation init values
      REG_OUTPUT => 0,     -- DECIMAL; 0=disable registered output, 1=enable registered output
      RST_USED => 0,       -- DECIMAL; 0=no reset, 1=implement reset
      SIM_ASSERT_CHK => 0  -- DECIMAL; 0=disable simulation messages, 1=enable simulation messages
   )
   port map (
      dest_pulse => FF_Rd, -- 1-bit output: Outputs a pulse the size of one dest_clk period when a pulse
                                -- transfer is correctly initiated on src_pulse input. This output is
                                -- combinatorial unless REG_OUTPUT is set to 1.

      dest_clk => clk,     -- 1-bit input: Destination clock.
      dest_rst => '0',     -- 1-bit input: optional; required when RST_USED = 1
      src_clk => clk_pci,       -- 1-bit input: Source clock.
      src_pulse => FF_Wr,   -- 1-bit input: Rising edge of this signal initiates a pulse transfer to the
                                -- destination clock domain. The minimum gap between each pulse transfer must
                                -- be at the minimum 2*(larger(src_clk period, dest_clk period)). This is
                                -- measured between the falling edge of a src_pulse to the rising edge of the
                                -- next src_pulse. This minimum gap will guarantee that each rising edge of
                                -- src_pulse will generate a pulse the size of one dest_clk period in the
                                -- destination clock domain. When RST_USED = 1, pulse transfers will not be
                                -- guaranteed while src_rst and/or dest_rst are asserted.

      src_rst => '0'        -- 1-bit input: optional; required when RST_USED = 1
   );

Umulx: FOR F IN 0 TO (FLT_MUL-1) GENERATE
BEGIN

Ud0: IF (F = 0) GENERATE
BEGIN
Udup: PROCESS(clk)
BEGIN
IF (clk'event AND clk = '1') THEN
	cFDATA_Sel(FLT_Num*(F+1)-1 downto FLT_Num*F)	<= FF_Dout(FDATA_Size+FLT_Num-1 downto FDATA_Size);
	FF_Rd_r(F) 									    <= FF_Rd;
	IF (FF_Rd = '1' AND FF_Dout(8-1 downto 3) = F) THEN
		CFDATA_Wr(FLT_Num*(F+1)-1 downto FLT_Num*F) <= FF_Dout(FDATA_Size+FLT_Num-1 downto FDATA_Size);
	ELSE
		CFDATA_Wr(FLT_Num*(F+1)-1 downto FLT_Num*F) <= (others => '0');
	END IF;
	IF (Filter_Match(FLT_Num*(F+1)-1 downto FLT_Num*F) /= 0) THEN
	   Filter_Match_r(F)   <= '1';
	ELSE
	   Filter_Match_r(F)   <= '0';
	END IF;
	IF (Filter_Drop(FLT_Num*(F+1)-1 downto FLT_Num*F) /= 0) THEN
	   Filter_Drop_r(F)   <= '1';
	ELSE
	   Filter_Drop_r(F)   <= '0';
	END IF;
    CFDATA(FDATA_Size*(F+1)-1 downto FDATA_Size*F)    <= FF_Dout(FDATA_Size-1 downto 0);
    Decode_Done(F)                      <= Decode_Done_IN;
    IP_Av(F)     	                    <= IP_Av_IN;
    IP_SRC(128*(F+1)-1 downto 128*F)   	<= IP_SRC_IN;
    IP_DST(128*(F+1)-1 downto 128*F)   	<= IP_DST_IN;
    IP_PROTO(8*(F+1)-1 downto 8*F)    	<= IP_PROTO_IN;
    L4_Av(F)                            <= L4_Av_IN;
    L4_SRC(16*(F+1)-1 downto 16*F)      <= L4_SRC_IN;
    L4_DST(16*(F+1)-1 downto 16*F)      <= L4_DST_IN;
    --
END IF;
END PROCESS;
END GENERATE;

Udn0: IF (F /= 0) GENERATE
BEGIN
Udup: PROCESS(clk)
BEGIN
IF (clk'event AND clk = '1') THEN
	cFDATA_Sel(FLT_Num*(F+1)-1 downto FLT_Num*F)	<= cFDATA_Sel(FLT_Num*F-1 downto FLT_Num*(F-1));
	FF_Rd_r(F) 									    <= FF_Rd_r(F-1);
	IF (FF_Rd_r(F-1) ='1' AND cFDATA(FDATA_Size*(F-1)+8-1 downto FDATA_Size*(F-1)+3) = F) THEN
		CFDATA_Wr(FLT_Num*(F+1)-1 downto FLT_Num*F) <= cFDATA_Sel(FLT_Num*F-1 downto FLT_Num*(F-1));
	ELSE
		CFDATA_Wr(FLT_Num*(F+1)-1 downto FLT_Num*F) <= (others => '0');
	END IF;
	IF (Filter_Match(FLT_Num*(F+1)-1 downto FLT_Num*F) /= 0 OR Filter_Match_r(F-1) = '1') THEN -- this or prev matched
	   Filter_Match_r(F)   <= '1';
	ELSE
	   Filter_Match_r(F)   <= '0';
	END IF;
	IF (Filter_Drop(FLT_Num*(F+1)-1 downto FLT_Num*F) /= 0 OR Filter_Drop_r(F-1) = '1') THEN -- this or prev matched
	   Filter_Drop_r(F)   <= '1';
	ELSE
	   Filter_Drop_r(F)   <= '0';
	END IF;
    CFDATA(FDATA_Size*(F+1)-1 downto FDATA_Size*F)    <= CFDATA(FDATA_Size*F-1 downto FDATA_Size*(F-1));
    Decode_Done(F)                      <= Decode_Done(F-1);
    IP_Av(F)     	                    <= IP_Av(F-1);
    IP_SRC(128*(F+1)-1 downto 128*F)   	<= IP_SRC(128*F-1 downto 128*(F-1));
    IP_DST(128*(F+1)-1 downto 128*F)   	<= IP_DST(128*F-1 downto 128*(F-1));
    IP_PROTO(8*(F+1)-1 downto 8*F)    	<= IP_PROTO(8*F-1 downto 8*(F-1));
    L4_Av(F)                            <= L4_Av(F-1);
    L4_SRC(16*(F+1)-1 downto 16*F)      <= L4_SRC(16*F-1 downto 16*(F-1));
    L4_DST(16*(F+1)-1 downto 16*F)      <= L4_DST(16*F-1 downto 16*(F-1));
END IF;
END PROCESS;
END GENERATE;

UFlt: FOR I IN 0 TO (FLT_Num-1) GENERATE
BEGIN

Ug: aFilter_100G
	PORT MAP(
	Clk 		     => Clk,
	FDATA_IN 	     => cFDATA(FDATA_Size*(F+1)-1 downto FDATA_Size*F),
	FDATA_Wr   	     => cFDATA_Wr(FLT_Num*F+I),
	--
	Decode_Done 	=> Decode_Done(F), --: IN STD_LOGIC;
	IP_Av 			=> IP_Av(F), --: IN STD_LOGIC;
	IP_SRC 			=> IP_SRC(128*(F+1)-1 downto 128*F), --: IN STD_LOGIC_VECTOR(32-1 downto 0);
	IP_DST 			=> IP_DST(128*(F+1)-1 downto 128*F), --: IN STD_LOGIC_VECTOR(32-1 downto 0);
	IP_PROTO 		=> IP_PROTO(8*(F+1)-1 downto 8*F), --: IN STD_LOGIC_VECTOR(8-1 downto 0);
	L4_Av 			=> L4_Av(F), --: IN STD_LOGIC;
	L4_SRC 			=> L4_SRC(16*(F+1)-1 downto 16*F), --: IN STD_LOGIC_VECTOR(16-1 downto 0);
	L4_DST 			=> L4_DST(16*(F+1)-1 downto 16*F), --: IN STD_LOGIC_VECTOR(16-1 downto 0);
	--
	Filter_Done 	=> Filter_Done(FLT_Num*F+I),
	Filter_Match 	=> Filter_Match(FLT_Num*F+I),
	Filter_Drop 	=> Filter_Drop(FLT_Num*F+I)
	);
	
END GENERATE;

END GENERATE;

-- drop-filter has priority over pass
UOut: PROCESS(clk)
BEGIN
IF (clk'event AND clk = '1') THEN
Filter_Done_r      <= Filter_Done(FLT_Num*FLT_MUL-1);
Filter_Done_OUT    <= Filter_Done_r;
IF (Filter_Done_r = '1') THEN
	IF (Filter_Drop_r(FLT_MUL-1) = '1') THEN -- filter match for drop
		Filter_Match_OUT 	<= '0';
	ELSIF (Filter_Match_r(FLT_MUL-1) = '1') THEN -- filter match for pass
		Filter_Match_OUT 	<= '1';
	ELSE -- no match, just drop it
		Filter_Match_OUT 	<= '0';
	END IF;
END IF;
END IF;
END PROCESS;

END STRUCTURE;

library IEEE;
use IEEE.std_logic_1164.all;
USE IEEE.std_logic_UNSIGNED.ALL;
USE work.pkg_FLT100G.FDATA_Size;
USE work.tuw.all;
--

ENTITY aFilter_100G IS
	PORT (
		Clk 			: IN STD_LOGIC;
		--	
		FDATA_IN 		: IN STD_LOGIC_VECTOR(FDATA_Size-1 downto 0);
		FDATA_Wr 		: IN STD_LOGIC;
		--
		Decode_Done 	: IN STD_LOGIC;
		IP_Av 			: IN STD_LOGIC;
		IP_SRC 			: IN STD_LOGIC_VECTOR(16*8-1 downto 0);
		IP_DST 			: IN STD_LOGIC_VECTOR(16*8-1 downto 0);
		IP_PROTO 		: IN STD_LOGIC_VECTOR(8-1 downto 0);
		L4_Av 			: IN STD_LOGIC;
		L4_SRC 			: IN STD_LOGIC_VECTOR(16-1 downto 0);
		L4_DST 			: IN STD_LOGIC_VECTOR(16-1 downto 0);
		--
		Filter_Done 	: OUT STD_LOGIC;
		Filter_Match	: OUT STD_LOGIC;
		Filter_Drop		: OUT STD_LOGIC
		);
END aFilter_100G;

ARCHITECTURE STRUCTURE OF aFilter_100G IS 
--
SIGNAL FDATA_SIP_CTRL : STD_LOGIC_VECTOR(4-1 downto 0);
SIGNAL FDATA_DIP_CTRL : STD_LOGIC_VECTOR(4-1 downto 0);

SIGNAL FDATA 		: STD_LOGIC_VECTOR(FDATA_Size-1 downto 0);
SIGNAL FCTRL 		: STD_LOGIC_VECTOR(11-1 downto 0);
SIGNAL FDROP        : STD_LOGIC := '0';
SIGNAL FLT_Match 	: STD_LOGIC_VECTOR(11-1 downto 0);
SIGNAL Decode_Done_r : STD_LOGIC;

--
BEGIN

FDATA_SIP_CTRL  <= FDATA_IN(8) & FDATA_IN(8) &FDATA_IN(8) & FDATA_IN(8);
FDATA_DIP_CTRL  <= FDATA_IN(9) & FDATA_IN(9) &FDATA_IN(9) & FDATA_IN(9);

UFlt: PROCESS(clk)
BEGIN
IF (clk'event AND clk = '1') THEN
--
IF (FDATA_Wr = '1') THEN
	FCTRL(4-1 downto 0) 	<= NOT FDATA_SIP_CTRL;
	FCTRL(8-1 downto 4) 	<= NOT FDATA_DIP_CTRL;
	FCTRL(11-1 downto 8)    <= NOT FDATA_IN(13-1 downto 10);
    FDROP   <= FDATA_IN(16-1);
	FDATA 	<= FDATA_IN;
END IF;
IF (IP_Av = '1') THEN
    FOR S IN 0 TO (4-1) LOOP
	IF (FDATA(24+32*(S+1)-1 downto 24+(32*S)) = IP_SRC(32*(S+1)-1 downto 32*S)) THEN
		FLT_Match(S) 	<= '1';
	ELSE
		FLT_Match(S)	<= '0';
	END IF;
	END LOOP;
	
	FOR D IN 0 TO (4-1) LOOP
	IF (FDATA(152+32*(D+1)-1 downto 152+(32*D)) = IP_DST(32*(D+1)-1 downto 32*D)) THEN
		FLT_Match(4+D) 	<= '1';
	ELSE
		FLT_Match(4+D)	<= '0';
	END IF;
	END LOOP;
	
	IF (FDATA(24-1 downto 16) = IP_PROTO) THEN
		FLT_Match(8)	<= '1';
	ELSE
		FLT_Match(8) 	<= '0';
	END IF;
ELSE
	FLT_Match(8 downto 0)	<= "000000000";
END IF;
--
IF (L4_Av = '1') THEN
	IF (FDATA(280+16-1 downto 280) = L4_SRC) THEN
		FLT_Match(9)	<= '1';
	ELSE
		FLT_Match(9)	<= '0';
	END IF;
	IF (FDATA(296+16-1 downto 296) = L4_DST) THEN
		FLT_Match(10)	<= '1';
	ELSE
		FLT_Match(10)	<= '0';
	END IF;
ELSE
	FLT_Match(10 downto 9) 	<= "00";
END IF;
--
Decode_Done_r 	<= Decode_Done;
Filter_Done 	<= Decode_Done_r;
IF ((FLT_Match OR FCTRL) = "11111111111") THEN
	Filter_Match	<= '1';
    Filter_Drop     <= FDROP;
ELSE
	Filter_Match 	<= '0';
    Filter_Drop     <= '0';	
END IF;
--
END IF;
END PROCESS;


END STRUCTURE;