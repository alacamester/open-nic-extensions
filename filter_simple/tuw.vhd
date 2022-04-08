--This is a utilities package for TU developed FPGA's
library IEEE;
USE IEEE.std_logic_1164.ALL;
USE IEEE.std_logic_UNSIGNED.ALL;

PACKAGE tuw IS
  PROCEDURE HiZ (  SIGNAL v: OUT   STD_LOGIC_VECTOR );
  PROCEDURE Set(   SIGNAL v: OUT   STD_LOGIC_VECTOR );
  PROCEDURE Reset( SIGNAL v: OUT   STD_LOGIC_VECTOR );
  PROCEDURE Incr(  SIGNAL v: INOUT STD_LOGIC_VECTOR );
  PROCEDURE Decr(  SIGNAL v: INOUT STD_LOGIC_VECTOR );
  PROCEDURE Let(   SIGNAL v: OUT   STD_LOGIC_VECTOR; CONSTANT j: IN INTEGER );
  FUNCTION  Match( SIGNAL v: IN    STD_LOGIC_VECTOR;
                        val: IN    STD_LOGIC_VECTOR;
                       mask: IN    STD_LOGIC_VECTOR) RETURN BOOLEAN;
END tuw;
--
--
PACKAGE BODY tuw IS
PROCEDURE HiZ ( SIGNAL v: OUT STD_LOGIC_VECTOR ) IS
BEGIN
    FOR i IN v'RANGE LOOP
      v(i) <= 'Z';
    END LOOP;
END;
--
PROCEDURE Reset( SIGNAL v: OUT STD_LOGIC_VECTOR ) IS
BEGIN
    FOR i IN v'RANGE LOOP
      v(i) <= '0';
    END LOOP;
END;
--
PROCEDURE Set( SIGNAL v: OUT STD_LOGIC_VECTOR ) IS
BEGIN
    FOR i IN v'RANGE LOOP
      v(i) <= '1';
    END LOOP;
END;
--
PROCEDURE Incr( SIGNAL v: INOUT STD_LOGIC_VECTOR ) IS
BEGIN
    v <= v + 1;
END;
--
PROCEDURE Decr( SIGNAL v: INOUT STD_LOGIC_VECTOR ) IS
BEGIN
    v <= v - 1;
END;
--
PROCEDURE Let( SIGNAL v: OUT STD_LOGIC_VECTOR; CONSTANT j: IN INTEGER ) IS
VARIABLE count: INTEGER;
BEGIN
    count := j;
    FOR i IN v'RIGHT TO v'LEFT LOOP
       IF (count mod 2 = 1) THEN
          v(i) <= '1';
       ELSE
          v(i) <= '0';
       END IF;
       count := (count / 2);
    END LOOP;
END Let;
--
FUNCTION Match( SIGNAL v: IN STD_LOGIC_VECTOR;
                     val: IN STD_LOGIC_VECTOR;
                    mask: IN STD_LOGIC_VECTOR
                     ) RETURN BOOLEAN IS
BEGIN
IF( (v AND mask) = (val AND mask) ) THEN RETURN TRUE;
ELSE                                     RETURN FALSE;
END IF;
--    FOR i IN v'RIGHT TO v'LEFT LOOP
--       IF (mask(i) = '1') THEN
--          IF( v(i) /= val(i)  ) THEN RETURN FALSE; END IF;
--       END IF;
--    END LOOP;
--RETURN TRUE;
END    Match;
--
END TUW;

