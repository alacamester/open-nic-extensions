-- functions_base.vhd
-- ==================
--
--	Common functions used in most projects
--
-- 2013.09.11 		- LK: created
--
library UNISIM;
use UNISIM.VCOMPONENTS.ALL;

LIBRARY IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.std_logic_UNSIGNED.all;
USE IEEE.numeric_std.ALL;
USE IEEE.math_real.ALL;
--
--      =================
PACKAGE FunctionsPkg_base IS
--      =================

--
---- Declare functions and procedures
--
	function nZero (
		constant	a		: in		integer
	) return string;

	function log2_ceil(
		N				: integer
	) return integer;

	function rot_left(
		signal	src		: std_logic_vector;
		constant	hm		: integer
	) return std_logic_vector;

	function rot_right(
		signal	src		: std_logic_vector;
		constant	hm		: integer
	) return std_logic_vector;

	function seekthe1_left(
		signal	src		: std_logic_vector
	) return integer;

	function seekthe1_right(
		signal	src		: std_logic_vector
	) return integer;

	function conv_exp2bin(
		signal	src		: std_logic_vector
	) return std_logic_vector;

	function conv_bin2exp(
		signal	src		: std_logic_vector
	) return integer;

	function and_vect2bit(
		signal	src		: std_logic_vector;
		signal	abit		: std_logic
	) return std_logic_vector;

	function or_vect2bit(
		signal	src		: std_logic_vector;
		signal	abit		: std_logic
	) return std_logic_vector;

	function conv_stdlogic(
		constant	a		: in integer;
		constant	b		: in integer
	) return std_logic_vector;

	FUNCTION maximum(
		left				: INTEGER;
		right			: INTEGER
	) RETURN INTEGER;

	FUNCTION absolute(
		val				: INTEGER
	) RETURN INTEGER;

	FUNCTION TO_BOOLEAN(
		L				: STD_LOGIC
	) return BOOLEAN;

	FUNCTION SwapOrder(
		SIGNAL	IN_vec	: IN		STD_LOGIC_VECTOR
	) return STD_LOGIC_VECTOR;

	FUNCTION NULL_vec(
		CONSTANT	IN_Len	: IN		INTEGER
	) return STD_LOGIC_VECTOR;

	FUNCTION ONE_vec(
		CONSTANT	IN_Len	: IN		INTEGER
	) return STD_LOGIC_VECTOR;

	FUNCTION getWidth(
		CONSTANT	Num		: IN		INTEGER
	) return INTEGER;

	FUNCTION getVec(
		CONSTANT	Num		: IN		INTEGER;
		CONSTANT	W		: IN		INTEGER
	) return STD_LOGIC_VECTOR;

	FUNCTION REALtoFIXED(
		CONSTANT	R		: IN		REAL;
		CONSTANT	W		: IN		INTEGER
	) return STD_LOGIC_VECTOR;

	FUNCTION CountUsefulBits(
		CONSTANT	V		: IN		STD_LOGIC_VECTOR
	) return INTEGER;

	PROCEDURE setVec(
		SIGNAL	v		: OUT	STD_LOGIC_VECTOR;
		CONSTANT	num		: IN		INTEGER
	);
	
	FUNCTION PadLeft(
		SIGNAL	padding	: IN		STD_LOGIC;
		SIGNAL	to_pad	: IN		STD_LOGIC_VECTOR;
		CONSTANT	len		: IN		INTEGER
	) RETURN STD_LOGIC_VECTOR;
	
	
	--source: http://www.stefanvhdl.com/vhdl/vhdl/txt_util.vhd
	-- functions to manipulate strings
	-----------------------------------
	-- convert a character to upper case
	function to_upper(
		c	: character
	) return character;

	-- convert a character to lower case
	function to_lower(
		c	: character
	) return character;

	-- convert a string to upper case
	function to_upper(
		s	: string
	) return string;

	-- convert a string to lower case
	function to_lower(
		s	: string
	) return string;

END FunctionsPkg_base;

--
PACKAGE BODY FunctionsPkg_base IS

	FUNCTION PadLeft(
		SIGNAL	padding	: IN		STD_LOGIC;
		SIGNAL	to_pad	: IN		STD_LOGIC_VECTOR;
		CONSTANT	len		: IN		INTEGER
	) RETURN STD_LOGIC_VECTOR IS
		VARIABLE retval	: STD_LOGIC_VECTOR(len - 1	DOWNTO 00 );
	BEGIN
		ASSERT len >= to_pad'LENGTH	REPORT "Invalid length"			SEVERITY FAILURE;
		ASSERT NOT to_pad'ASCENDING	REPORT "Invalid order of vector"	SEVERITY FAILURE;
		
		IF(len > to_pad'LENGTH)THEN
			retval(len - 1 DOWNTO to_pad'LENGTH)	:= ( OTHERS => padding);
		END IF;
		retval(to_pad'RANGE)	:= to_pad;
		RETURN retval;
	END FUNCTION;
	


-- Check if integer is zero
function nZero  (constant a : in integer )return string is
begin
	if (a = 0) THEN
      	return "FALSE";
   	else
      	return "TRUE";
   	end if;
end nZero;
--return log2 value of integer
function log2_ceil(N : integer)return integer is
begin
    if (N <= 2) then
 return 1;
    else
        if (N mod 2 = 0) then
     return 1 + log2_ceil(N/2);
        else
     return 1 + log2_ceil((N+1)/2);
        end if;
    end if;
end function log2_ceil;
-- Rotate left input vector by number of bits
function rot_left(signal src : std_logic_vector; constant hm: integer)return std_logic_vector is
begin
return std_logic_vector(unsigned(src) rol hm);
end function rot_left;
-- Rotate left input vector by number of bits
function rot_right(signal src : std_logic_vector; constant hm: integer)return std_logic_vector is
begin
return std_logic_vector(unsigned(src) ror hm);
end function rot_right;
-- Seek the position of the first '1' value from left
function seekthe1_left(signal src : std_logic_vector)return integer is
variable aval : integer := 0;
begin
for I in src'left downto 0 loop
	aval := aval+1;
	if (src(I) = '1') then
		return aval;
	end if;
end loop;
return 0;
end function seekthe1_left;
-- Seek the position of the first '1' value from right
function seekthe1_right(signal src : std_logic_vector)return integer is
variable aval : integer := 0;
begin
for I in 0 to src'left loop
	aval := aval+1;
	if (src(I) = '1') then
		return aval;
	end if;
end loop;
return 0;
end function seekthe1_right;
--return 2exp value of input vector
function conv_exp2bin(signal src : std_logic_vector)return std_logic_vector is
constant aexp : integer := conv_integer(src);
variable result : std_logic_vector(2**src'length-1 downto 0) := (others => '0');
begin
for I in 0 to result'length-1 loop
	if (aexp = I) then
		result(I)	:= '1';
	else
		result(I)	:= '0';
	end if;
end loop;
return result;
end function conv_exp2bin;
--return log2 value of input vector (position nr.)
function conv_bin2exp(signal src : std_logic_vector)return integer is
variable aval : integer := 0;
begin
for I in 0 to src'length-1 loop
	aval := aval + 1;
	if (src(I) = '1') then
		return aval;
	end if;
end loop;
return 1;
end function conv_bin2exp;
-- And vector to a bit value
function and_vect2bit(signal src : std_logic_vector; signal abit: std_logic)return std_logic_vector is
variable result : std_logic_vector(src'length-1 downto 0);
begin
for I in 0 to src'length-1 loop
	result(I)	:= src(I) AND abit;
end loop;
return result;
end function and_vect2bit;
-- Or vector to a bit value
function or_vect2bit(signal src : std_logic_vector; signal abit: std_logic)return std_logic_vector is
variable result : std_logic_vector(src'length-1 downto 0);
begin
for I in 0 to src'length-1 loop
	result(I)	:= src(I) OR abit;
end loop;
return result;
end function or_vect2bit;
-- Convert integer to std_logic_vector (output vector size included)
function conv_stdlogic(constant a : in integer; constant b : in integer)return std_logic_vector is
variable result : std_logic_vector(b-1 downto 0);
begin
result := std_logic_vector(to_unsigned(a, result'length));
return result;
end function conv_stdlogic;



	FUNCTION CountUsefulBits(	CONSTANT	V		: IN		STD_LOGIC_VECTOR) RETURN INTEGER IS
		VARIABLE retval	: INTEGER;
	BEGIN
		
		retval := 0;
		IF(V'ASCENDING)THEN
			FOR I IN V'RANGE LOOP
				IF(V(I) = '1')THEN
					retval := I+1;
				END IF;
			END LOOP;
		ELSE
			FOR I IN V'RANGE LOOP
				IF(V(I) = '1')THEN
					retval := I+1;
					exit;
				END IF;
			END LOOP;
		END IF;
		RETURN retval;
	END;

	FUNCTION getVec(	CONSTANT	Num		: IN		INTEGER;
					CONSTANT	W		: IN		INTEGER			) RETURN STD_LOGIC_VECTOR IS
	BEGIN
		ASSERT Num >= 0 REPORT "Num must be greater or equal than 0, num is:"&INTEGER'IMAGE(Num) SEVERITY FAILURE;
		ASSERT W >= 1 REPORT "W must be greater or equal than 1, w is:"&INTEGER'IMAGE(W) SEVERITY FAILURE;
		RETURN STD_LOGIC_VECTOR(TO_UNSIGNED(Num,W));
	END;
	
	FUNCTION REALtoFIXED(	CONSTANT	R		: IN		REAL;
						CONSTANT	W		: IN		INTEGER			) RETURN STD_LOGIC_VECTOR IS
		VARIABLE Integer_part	: INTEGER;
		VARIABLE temp			: REAL;
		VARIABLE div			: REAL;
		VARIABLE Rabs			: REAL;
		VARIABLE neg_flag		: BOOLEAN;
		VARIABLE retval		: STD_LOGIC_VECTOR( W-1 DOWNTO 0);
		CONSTANT null_vec		: STD_LOGIC_VECTOR(retval'RANGE)	:= getVec(0,retval'LENGTH);
	BEGIN
		ASSERT W MOD 2 = 0 REPORT "W must be EVEN!!!R is :" & REAL'IMAGE(R*1.0e9) &" ; W is : " & INTEGER'IMAGE(W) SEVERITY FAILURE;
	
		IF(R <0.0)THEN
			neg_flag	:= TRUE;
			Rabs	:= -1.0*R;
		ELSE
			neg_flag	:= FALSE;
			Rabs	:= R;
		END IF;
		Integer_part			:= INTEGER(FLOOR(Rabs));
		temp					:= Rabs - REAL(Integer_part);
		-- temp					:= temp*2.0**W/2.0;
		-- temp					:= R-REAL(Integer_part);
		-- REPORT INTEGER'IMAGE(Integer_part);
		-- REPORT INTEGER'IMAGE(W/2);
		retval(W-1 DOWNTO W/2)	:=  STD_LOGIC_VECTOR(TO_UNSIGNED(Integer_part,W/2));
		-- retval(W/2-1 DOWNTO 0)	:= getVec(INTEGER(ROUND(temp)),W/2);
		FOR I IN 0 TO W/2-1 LOOP
			-- REPORT INTEGER'IMAGE(I);
			div	:= temp * (2.0**(I+1));

			IF( div >= 1.0)THEN
				retval(W/2-1-I)	:= '1';
				temp				:= temp - 1.0/(2.0**(I+1));
			ELSE
				retval(W/2-1-I)	:= '0';
			END IF;
		END LOOP;
		IF(neg_flag)THEN
			retval := NOT(RETVAL) + 1;
		END IF;
		ASSERT ((retval = null_vec) AND (R = 0.0)) OR (retval /= null_vec)
			REPORT "Not enough width for this number representation : R = " & REAL'IMAGE(R) & " , W = " & INTEGER'IMAGE(W) SEVERITY WARNING;
		RETURN retval;
	END;

	FUNCTION getWidth(	CONSTANT	Num		: IN		INTEGER			) RETURN INTEGER IS
	BEGIN
		RETURN INTEGER(CEIL(LOG2(REAL(maximum(2,Num)))));
	END;
	PROCEDURE setVec(	SIGNAL	v		: OUT	STD_LOGIC_VECTOR;
					CONSTANT	num		: IN		INTEGER			)IS
	BEGIN
		v	<= STD_LOGIC_VECTOR(TO_UNSIGNED(num,v'LENGTH));
	END;
	
	FUNCTION SwapOrder(			SIGNAL	IN_vec			: IN	STD_LOGIC_VECTOR	) RETURN STD_LOGIC_VECTOR IS
		VARIABLE retval	: STD_LOGIC_VECTOR(IN_vec'RANGE);
		CONSTANT InvecLeft	: INTEGER	:= IN_vec'LEFT;
		CONSTANT InvecRight	: INTEGER	:= IN_vec'RIGHT;
	BEGIN
		IF(IN_vec'LENGTH = 1)THEN
			retval	:= IN_vec;
		ELSE
			FOR I in IN_vec'RANGE LOOP
				IF(IN_vec'ASCENDING)THEN
					retval(I)	:= IN_vec(IN_vec'RIGHT-IN_vec'LENGTH + 1 + (IN_vec'RIGHT-I));
				ELSE
					retval(I)	:= IN_vec(IN_vec'LEFT-IN_vec'LENGTH + 1 + (IN_vec'LEFT-I));
				END IF;
			END LOOP;
		END IF;
		RETURN retval;
	END;

	FUNCTION NULL_vec(	CONSTANT	IN_Len	: IN		INTEGER			) RETURN STD_LOGIC_VECTOR IS
	BEGIN
		RETURN STD_LOGIC_VECTOR(TO_UNSIGNED(0,IN_Len));
	END;
	
	FUNCTION ONE_vec(	CONSTANT	IN_Len	: IN		INTEGER			) RETURN STD_LOGIC_VECTOR IS
		variable	retval	: STD_LOGIC_VECTOR(IN_Len -1 DOWNTO 0) := (OTHERS => '1');
	BEGIN
		RETURN retval;
	END;
function absolute(
	val : INTEGER)
	return INTEGER IS
BEGIN
	IF val < 0 THEN return -1*val;
	ELSE return val;
	END IF;
END function absolute;
--BEGIN
-- functions here
--
--source:http://vhdl.org/vhdlsynth/vhdl/minmax.vhd
	FUNCTION maximum(
		left				: INTEGER;
		right			: INTEGER
	) RETURN INTEGER IS
 BEGIN  -- function max
	IF LEFT > RIGHT THEN return LEFT;
	ELSE return RIGHT;
	END IF;
 END function maximum;
 

	function TO_BOOLEAN(L: STD_LOGIC) return BOOLEAN is
	begin
		if L = '1' then
			return(TRUE);
		else
			return(FALSE);
		end if;
	end function TO_BOOLEAN;
	
 -- convert a character to upper case

	function to_upper(c: character) return character is

		variable u: character;

	begin

		case c is
		when 'a' => u := 'A';
		when 'b' => u := 'B';
		when 'c' => u := 'C';
		when 'd' => u := 'D';
		when 'e' => u := 'E';
		when 'f' => u := 'F';
		when 'g' => u := 'G';
		when 'h' => u := 'H';
		when 'i' => u := 'I';
		when 'j' => u := 'J';
		when 'k' => u := 'K';
		when 'l' => u := 'L';
		when 'm' => u := 'M';
		when 'n' => u := 'N';
		when 'o' => u := 'O';
		when 'p' => u := 'P';
		when 'q' => u := 'Q';
		when 'r' => u := 'R';
		when 's' => u := 'S';
		when 't' => u := 'T';
		when 'u' => u := 'U';
		when 'v' => u := 'V';
		when 'w' => u := 'W';
		when 'x' => u := 'X';
		when 'y' => u := 'Y';
		when 'z' => u := 'Z';
		when others => u := c;
	end case;

		return u;

	end to_upper;


	-- convert a character to lower case

	function to_lower(c: character) return character is

		variable l: character;

	begin

		case c is
		when 'A' => l := 'a';
		when 'B' => l := 'b';
		when 'C' => l := 'c';
		when 'D' => l := 'd';
		when 'E' => l := 'e';
		when 'F' => l := 'f';
		when 'G' => l := 'g';
		when 'H' => l := 'h';
		when 'I' => l := 'i';
		when 'J' => l := 'j';
		when 'K' => l := 'k';
		when 'L' => l := 'l';
		when 'M' => l := 'm';
		when 'N' => l := 'n';
		when 'O' => l := 'o';
		when 'P' => l := 'p';
		when 'Q' => l := 'q';
		when 'R' => l := 'r';
		when 'S' => l := 's';
		when 'T' => l := 't';
		when 'U' => l := 'u';
		when 'V' => l := 'v';
		when 'W' => l := 'w';
		when 'X' => l := 'x';
		when 'Y' => l := 'y';
		when 'Z' => l := 'z';
		when others => l := c;
	end case;

		return l;

	end to_lower;



	-- convert a string to upper case

	function to_upper(s: string) return string is

	variable uppercase: string (s'range);

	begin

	for i in s'range loop
		uppercase(i):= to_upper(s(i));
	end loop;
	return uppercase;

	end to_upper;



	-- convert a string to lower case

	function to_lower(s: string) return string is

	variable lowercase: string (s'range);

	begin

	for i in s'range loop
		lowercase(i):= to_lower(s(i));
	end loop;
	return lowercase;

	end to_lower;


--
END FunctionsPkg_base;
--