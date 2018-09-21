----------------------------------------------------------------------------
--	GPIO_Demo.vhd -- Nexys4 GPIO/UART Demonstration Project
----------------------------------------------------------------------------
-- Author:  Bertrand LE GAL
--          Copyright 2018 Bordeaux-INP (ENSEIRB-MATMECA)
----------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;

entity GPIO_demo is
    Port ( --SW 			: in  STD_LOGIC_VECTOR (3 downto 0); -- LE 0:ENABLE, le 15:RESET
        CLK 			: in  STD_LOGIC;
        RESET		: in  STD_LOGIC;
        ampPWM       : out STD_LOGIC;
        ampSD        : out STD_LOGIC;
        LED      : out STD_LOGIC_VECTOR(15 downto 0);                      -- DATA line for SDcard access
        SD_DAT   : inout STD_LOGIC_VECTOR( 3 downto 0);                      -- DATA line for SDcard access
        SD_CD    : in    STD_LOGIC;                                          -- SDcard detected (active low)
        SD_CLK   : out   STD_LOGIC;   
        SD_RESET : out   STD_LOGIC;   
        SD_CMD   : inout STD_LOGIC                                         -- Command line
	);
end GPIO_demo;

architecture Behavioral of GPIO_demo is
    SIGNAL BASE_TEMPS   : STD_LOGIC;
    SIGNAL SOUND_VALUES : STD_LOGIC_VECTOR (31 downto 0);
    
    SIGNAL data_empty_n : STD_LOGIC;
    SIGNAL debug        : STD_LOGIC_VECTOR( 5 downto 0);
    SIGNAL sd_error     : STD_LOGIC;

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
        clk          => CLK,
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

    LED <= SOUND_VALUES(15 downto 0) WHEN RESET ='0' ELSE x"FFFF";

	timer : ENTITY WORK.time_base 
    PORT MAP(
		RESET    => RESET,
		CLOCK    => clk,
		TIME_REF => BASE_TEMPS);

    system : ENTITY WORK.pwm_mod
    PORT MAP(
        RESET     => RESET,
        CLOCK     => CLK,
        EXT_CLOCK => BASE_TEMPS,
        DATA_IN   => SOUND_VALUES(15 downto 5),
        ampPWM    => ampPWM,
        ampSD     => ampSD
	);

end Behavioral;
