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
    Port (
        CLK 	    : in  STD_LOGIC;
        RESET		: in  STD_LOGIC;
        source      : in STD_LOGIC;
        ampPWM       : out STD_LOGIC;
        ampSD        : out STD_LOGIC;
        LED      : out STD_LOGIC_VECTOR(15 downto 0);                      -- DATA line for SDcard access

      -- VGA display
      vga_hs_o       : out std_logic;
      vga_vs_o       : out std_logic;
      vga_red_o      : out std_logic_vector(3 downto 0);
      vga_blue_o     : out std_logic_vector(3 downto 0);
      vga_green_o    : out std_logic_vector(3 downto 0);

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


    SIGNAL CPT_1 : INTEGER RANGE 0 TO 10000000 := 0;
    SIGNAL CPT_2 : INTEGER RANGE 0 TO 375 := 0;

--    attribute mark_debug : string;
--    attribute mark_debug of iCLK: signal is "true";
--    attribute mark_debug of BITs: signal is "true";
--    attribute mark_debug of REGs: signal is "true";

    SIGNAL VALUE   : STD_LOGIC_VECTOR (15 downto 0);
    SIGNAL VALU2   : STD_LOGIC_VECTOR (15 downto 0) := (OTHERS => '0');
    SIGNAL VALUE_E : STD_LOGIC;

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
        VARIABLE T1, T2 : SIGNED(15 DOWNTO 0);
    begin
        if rising_edge(clk) then
            if source='1' then
                T1 := SIGNED(VAL_FROM_SD(31 downto 16)) + TO_SIGNED(32767, 16);
                T2 := SIGNED(VAL_FROM_SD(15 downto  0)) + TO_SIGNED(32767, 16);
                SOUND_VALUES <= STD_LOGIC_VECTOR(T1 & T2);
            else
                SOUND_VALUES <= (31 downto 30 => VAL_FROM_SD(31)) & VAL_FROM_SD(31 downto 18) & (15 downto 14 => VAL_FROM_SD(15)) & VAL_FROM_SD(15 downto 2);
            end if;
        end if;
    end process;
    
    process(clk)
        VARIABLE T1, T2 : SIGNED(15 DOWNTO 0);
    begin
        if rising_edge(clk) then
            if CPT_1 /= (10000000-1) then
                CPT_1   <= CPT_1 + 1;
                CPT_2   <= CPT_2;
                VALUE_E <= '0';
            else
                CPT_1   <= 0;
                if CPT_2 /= 375 then
                    CPT_2   <= CPT_2 + 1;
                ELSE
                    CPT_2   <= 0;
                END IF;
                VALUE_E <= '1';
            end if;
        end if;
    end process;
    
    VALUE <= STD_LOGIC_VECTOR( TO_UNSIGNED(CPT_2, 16) );

--	timer : ENTITY WORK.time_base 
--    PORT MAP(
--		RESET    => RESET,
--		CLOCK    => clk,
--		TIME_REF => BASE_TEMPS);

    system : ENTITY WORK.pwm_mod
    PORT MAP(
        RESET     => RESET,
        CLOCK     => clk,
        EXT_CLOCK => BASE_TEMPS,
        DATA_IN   => SOUND_VALUES(15 downto 5),
        ampPWM    => ampPWM,
        ampSD     => ampSD
	);

    sin : ENTITY WORK.sine_wave
    PORT MAP(
        RESET     => RESET,
        CLOCK     => clk,
        ENABLE    => VALUE_E,
        wave_out  => VALU2(8 DOWNTO 0)
	);

    I2S_audio : Pmod_I2S_out 
        Port map(clk    => clk,
                 reset  => reset,
                 left   => SOUND_VALUES(15 downto  0),
                 right  => SOUND_VALUES(31 downto 16),
                 --sync   => open,
                 sync   => BASE_TEMPS,
                 MCLK   => MCLK,
                 SCLK   => SCK,
                 SDIN   => SDIN,
                 LRCK   => LRCK);

   	
   Inst_VGA: ENTITY work.Vga
                 port map(
                    clk_i            => clk,
                    vga_hs_o         => vga_hs_o,
                    vga_vs_o         => vga_vs_o,
                    vga_red_o        => vga_red_o,
                    vga_blue_o       => vga_blue_o,
                    vga_green_o      => vga_green_o,
                    MIC_M_DATA_I_1     => VALUE, --SOUND_VALUES(15 downto 0),
                    MIC_M_CLK_RISING_1 => VALUE_E, --BASE_TEMPS
                    MIC_M_DATA_I_2     => VALU2, --SOUND_VALUES(15 downto 0),
                    MIC_M_CLK_RISING_2 => VALUE_E --BASE_TEMPS
                    );
    

end Behavioral;
