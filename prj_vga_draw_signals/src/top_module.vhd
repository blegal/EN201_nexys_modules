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

      -- VGA display
      vga_hs_o       : out std_logic;
      vga_vs_o       : out std_logic;
      vga_red_o      : out std_logic_vector(3 downto 0);
      vga_blue_o     : out std_logic_vector(3 downto 0);
      vga_green_o    : out std_logic_vector(3 downto 0)
	);
end top_module;

architecture Behavioral of top_module is

    SIGNAL BASE_TEMPS   : STD_LOGIC;
    SIGNAL VAL_FROM_SD  : STD_LOGIC_VECTOR (31 downto 0);
    SIGNAL SOUND_VALUES : STD_LOGIC_VECTOR (31 downto 0);
    
    SIGNAL data_empty_n : STD_LOGIC;
    SIGNAL debug        : STD_LOGIC_VECTOR( 5 downto 0);
    SIGNAL sd_error     : STD_LOGIC;


    SIGNAL CPT_1 : INTEGER RANGE 0 TO 10000000 := 0;
    SIGNAL CPT_2 : INTEGER RANGE 0 TO 375 := 0;

    SIGNAL VALUE   : STD_LOGIC_VECTOR (15 downto 0);
    SIGNAL VALU2   : STD_LOGIC_VECTOR (15 downto 0) := (OTHERS => '0');
    SIGNAL VALUE_E : STD_LOGIC;

BEGIN

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

    sin : ENTITY WORK.sine_wave
    PORT MAP(
        RESET     => RESET,
        CLOCK     => clk,
        ENABLE    => VALUE_E,
        wave_out  => VALU2(8 DOWNTO 0)
	);

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
