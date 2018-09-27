LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY time_base IS
PORT (
      RESET    : IN  STD_LOGIC;
      CLOCK    : IN  STD_LOGIC;
      TIME_REF : OUT STD_LOGIC
      );
END time_base;

ARCHITECTURE Behavioral OF time_base IS
	CONSTANT max_value : INTEGER := 2267;
	SIGNAL   counter   : INTEGER RANGE 0 TO 2268 := 0;
BEGIN

    PROCESS(RESET, CLOCK)
    BEGIN
        IF RESET = '1' THEN
            counter <= 0;

        ELSIF (rising_edge(CLOCK)) then
            IF ( counter = max_value ) THEN
                counter  <=  0;
                TIME_REF <= '1';
            ELSE
                counter  <=  counter + 1;
                TIME_REF <= '0';
            END IF;
        END IF;
	END PROCESS;

END Behavioral;
