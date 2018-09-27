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

entity top_module is
    Port (
        CLK 	    : in  STD_LOGIC;
        RESET		: in  STD_LOGIC;
        source      : in STD_LOGIC;

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
end top_module;

architecture Behavioral of top_module is

    Component Pmod_I2S_out is
        Port ( clk         : in  STD_LOGIC;
               reset       : in  STD_LOGIC;
               left        : in  STD_LOGIC_VECTOR (15 downto 0);
               right       : in  STD_LOGIC_VECTOR (15 downto 0);
               sync        : out STD_LOGIC;
               MCLK        : out STD_LOGIC;
               SCLK        : out STD_LOGIC;
               SDIN        : out STD_LOGIC;
               LRCK        : out STD_LOGIC);
    end component;


    SIGNAL BASE_TEMPS   : STD_LOGIC;
    SIGNAL VAL_FROM_SD  : STD_LOGIC_VECTOR (31 downto 0);
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
        HIGH_SPEED_IF   => True,
        LITTLE_ENDIAN   => True
    )
    PORT MAP (
        clk          => CLK,
        reset        => RESET,
        data_out     => VAL_FROM_SD,
        data_read    => BASE_TEMPS,
        data_empty_n => data_empty_n,
        debug        => debug,
        sd_error     => sd_error,
        SD_DAT       => SD_DAT,
        SD_CD        => SD_CD,
        SD_CLK       => SD_CLK,
        SD_CMD       => SD_CMD);

    LED <= SOUND_VALUES(15 downto 8) & debug & sd_error & data_empty_n;

    process(clk)
    begin
        if rising_edge(clk) then
            if source='1' then
                SOUND_VALUES <= VAL_FROM_SD;
            else
                SOUND_VALUES <= (31 downto 30 => VAL_FROM_SD(31)) & VAL_FROM_SD(31 downto 18) & (15 downto 14 => VAL_FROM_SD(15)) & VAL_FROM_SD(15 downto 2);
            end if;
        end if;
    end process;

    I2S_audio : Pmod_I2S_out 
        Port map(clk    => clk,
                 reset  => reset,
                 left   => SOUND_VALUES(15 downto  0),
                 right  => SOUND_VALUES(31 downto 16),
                 sync   => BASE_TEMPS,
                 MCLK   => MCLK,
                 SCLK   => SCK,
                 SDIN   => SDIN,
                 LRCK   => LRCK);

end Behavioral;
