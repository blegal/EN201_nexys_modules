LIBRARY IEEE;
USE IEEE.STD_LOGIC_1164.ALL;
USE IEEE.NUMERIC_STD.ALL;

ENTITY pwm_mod IS
PORT (
      RESET     : IN  STD_LOGIC;
      CLOCK     : IN  STD_LOGIC;
      DATA_IN   : IN  STD_LOGIC_VECTOR (10 DOWNTO 0);
      EXT_CLOCK : IN  STD_LOGIC;
      ampPWM    : OUT STD_LOGIC;
      ampSD     : OUT STD_LOGIC
      );
END pwm_mod;

ARCHITECTURE Behavioral OF pwm_mod IS
	CONSTANT max_value  : UNSIGNED(11 downto 0) := to_unsigned(2267, 12);
	SIGNAL   NB_TICKS   : UNSIGNED(11 downto 0) := (others=>'0');
	SIGNAL   counter    : UNSIGNED(11 downto 0) := (others=>'0');
	SIGNAL pwm_val_reg  : STD_LOGIC := '0';
BEGIN

    PROCESS(CLOCK)
    BEGIN
	   IF (rising_edge(CLOCK)) then
           IF EXT_CLOCK = '1' THEN
               NB_TICKS <= UNSIGNED(TO_SIGNED(1024, 12) + SIGNED(DATA_IN));
           ELSE
               NB_TICKS <= NB_TICKS;
		   END IF;
       END IF;
    END PROCESS;

    PROCESS(RESET, CLOCK)
    BEGIN
	   IF RESET = '1' THEN
	       counter     <= TO_UNSIGNED(0, 12);
	   ELSIF (rising_edge(CLOCK)) then
		   IF EXT_CLOCK = '1' THEN
		      counter  <= TO_UNSIGNED(0, 12);
		   ELSE
		      counter  <= counter + TO_UNSIGNED(1, 1);
		   END IF;
		END IF;
    END PROCESS;

    pwm_val_reg <= '1' WHEN counter < NB_TICKS ELSE '0';
	ampPWM      <= pwm_val_reg;
	ampSD       <= '1';

END Behavioral;
