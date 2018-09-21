----------------------------------------------------------------------------
--	GPIO_Demo.vhd -- Nexys4 GPIO/UART Demonstration Project
----------------------------------------------------------------------------
-- Author:  Bertrand LE GAL
--          Copyright 2018 Bordeaux-INP (ENSEIRB-MATMECA)
----------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;
use ieee.std_logic_misc.all;

entity GPIO_demo is
    Port ( --SW 			: in  STD_LOGIC_VECTOR (3 downto 0); -- LE 0:ENABLE, le 15:RESET
        CLK 			: in  STD_LOGIC;
        RESET		: in  STD_LOGIC;
        ampPWM       : out STD_LOGIC;
        ampSD        : out STD_LOGIC;
        LED      : out STD_LOGIC_VECTOR(15 downto 0);                      -- DATA line for SDcard access

        MCLK   : out   STD_LOGIC; -- 45.158 MHz
        LRCK   : out   STD_LOGIC; --  1 * 44100
        SCK    : out   STD_LOGIC; -- 32 * 44100
        SDIN   : out   STD_LOGIC;   
  
        SD_DAT   : inout STD_LOGIC_VECTOR( 3 downto 0);                      -- DATA line for SDcard access
        SD_CD    : in    STD_LOGIC;                                          -- SDcard detected (active low)
        SD_CLK   : out   STD_LOGIC;   
        SD_RESET : out   STD_LOGIC;   
        SD_CMD   : inout STD_LOGIC                                         -- Command line
	);
end GPIO_demo;

architecture Behavioral of GPIO_demo is


    COMPONENT clk_wiz_0 is
    Port ( --SW 			: in  STD_LOGIC_VECTOR (3 downto 0); -- LE 0:ENABLE, le 15:RESET
        CLK_IN1  : in    STD_LOGIC;                                          -- SDcard detected (active low)
        LOCKED   : out   STD_LOGIC;
        CLK_OUT1 : out   STD_LOGIC;  
        CLK_OUT2 : out   STD_LOGIC   
	);
    END COMPONENT;

    SIGNAL BASE_TEMPS   : STD_LOGIC;
    SIGNAL SOUND_VALUES : STD_LOGIC_VECTOR (31 downto 0);
    
    SIGNAL data_empty_n : STD_LOGIC;
    SIGNAL debug        : STD_LOGIC_VECTOR( 5 downto 0);
    SIGNAL sd_error     : STD_LOGIC;
    SIGNAL pCLK         : STD_LOGIC;
    SIGNAL fCLK         : STD_LOGIC;
    SIGNAL iCLK         : STD_LOGIC;
    SIGNAL LOCKED         : STD_LOGIC;

    SIGNAL REGs : STD_LOGIC_VECTOR (31 downto 0);
    SIGNAL BITs : STD_LOGIC_VECTOR (31 downto 0);
    SIGNAL fCOUNTER : INTEGER RANGE 0 TO 1023;

--    attribute mark_debug : string;
--    attribute mark_debug of iCLK: signal is "true";
--    attribute mark_debug of BITs: signal is "true";
--    attribute mark_debug of REGs: signal is "true";
   
BEGIN

    SD_RESET <= '0';

    card : ENTITY WORK.Sdcard_readstream
    GENERIC MAP(
        CLK_FREQ_HZ     => 100000000,
        VIVADO_SYNTH    => True,
        WORD_SIZE_PW2   => 2,
        HIGH_SPEED_IF   => False,
        LITTLE_ENDIAN   => True
    )
    PORT MAP (
        clk          => pCLK,
        reset        => RESET,
        data_out     => SOUND_VALUES,
        data_read    => BASE_TEMPS,
        data_empty_n => data_empty_n,
        debug        => debug,
        sd_error     => sd_error,
        SD_DAT       => SD_DAT,
        SD_CD        => SD_CD,
        SD_CLK       => SD_CLK,
        SD_CMD       => SD_CMD);

    LED <= SOUND_VALUES(15 downto 0) WHEN RESET ='0' ELSE x"FFF" & LOCKED & LOCKED & LOCKED & LOCKED;

	timer : ENTITY WORK.time_base 
    PORT MAP(
		RESET    => RESET,
		CLOCK    => pCLK,
		TIME_REF => BASE_TEMPS);

    system : ENTITY WORK.pwm_mod
    PORT MAP(
        RESET     => RESET,
        CLOCK     => pCLK,
        EXT_CLOCK => BASE_TEMPS,
        DATA_IN   => SOUND_VALUES(15 downto 5),
        ampPWM    => ampPWM,
        ampSD     => ampSD
	);
	
	clock : clk_wiz_0
	PORT MAP(
	       clk_in1  =>    CLK,
	       LOCKED   => LOCKED,
	       clk_out1 =>   pCLK,
	       clk_out2 =>   iCLK);
    
    MCLK   <= iCLK; -- 45.1580 MHz
    LRCK   <= NOT BITs(15);
    SCK    <= fCLK;

    --
    --
    --
    
    PROCESS(iCLK)
    BEGIN
        IF iCLK'EVENT AND iCLK = '0' THEN
            IF fCOUNTER = 31 THEN -- ???
                fCOUNTER <= 0;
                fCLK     <= NOT fCLK;
            ELSE
                fCOUNTER <= fCOUNTER + 1;
                fCLK     <= fCLK;
            END IF;
        END IF;
    END PROCESS;    
    
    PROCESS(fCLK)
        VARIABLE ZERO : STD_LOGIC_VECTOR(31 DOWNTO 0) := (OTHERS => '0');
    BEGIN
        IF fCLK'EVENT AND fCLK = '0' THEN
            IF BITs = ZERO THEN
                REGs <= SOUND_VALUES;
                BITs <= (OTHERS => '1');
                SDIN <= REGs(31);
            ELSE
                REGs <= REGs(30 DOWNTO 0) & '0';
                BITs <= BITs(30 DOWNTO 0) & '0';
                SDIN <= REGs(31);
            END IF;
        END IF;
    END PROCESS;	

end Behavioral;
