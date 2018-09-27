----------------------------------------------------------------------------------
-- Pmod_I2S_out
--     version 1.0.0 (2018/09/23) by YB
--    (c)2018 Y. BORNAT - Bordeaux INP / ENSEIRB-MATMECA
--
-- This module is intended to control the I2S audio output pmod.
-- It has its own clock generator and outputs a sync signal to
-- update the samples.
----------------------------------------------------------------------------------
-- V1.0.0 (2018/09/23) by YB
--    initial release
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


entity Pmod_I2S_out is
    Generic (CLK_FREQ : integer := 100000000;    -- Main clock frequency 
             OUT_FREQ : integer :=     44100);   -- between 2 and 50 kHz, must be =< CLK_FREQ/2048
    Port ( clk          : in  STD_LOGIC;                        -- main clock
           reset        : in  STD_LOGIC;                        -- system reset
           left         : in  STD_LOGIC_VECTOR (15 downto 0);   -- left channel data (unbuffered)
           right        : in  STD_LOGIC_VECTOR (15 downto 0);   -- right channel data (unbuffered)
           sync         : out STD_LOGIC;                        -- sync/read output, can be used to trigger data source for new sample
           MCLK         : out STD_LOGIC;                        -- Pmod MCLK
           SCLK         : out STD_LOGIC;                        -- Pmod SCLK
           SDIN         : out STD_LOGIC;                        -- Pmod SDIN
           LRCK         : out STD_LOGIC);                       -- Pmod LRCK
end Pmod_I2S_out;

architecture Behavioral of Pmod_I2S_out is

-- To manage the timebase, we increment clkdiv_cnt by clkdiv_inc until it reaches clk_freq. Once clk_freq
-- is reached, we subtract clk_freq from clkdiv_cnt and we assert clk_trig. clk_trig events are then produced
-- at the correct frequency.
-- This behavior is a bit out of specification because it produces local jitter on the clocks, and the 
-- cyclic ratio is from 44% to 56% (the datasheet specifies that it should be between 45% and 55%). The cyclic
-- ratio range becomes 42.5% - 57.5% if CLK_FREQ < OUT_FREQ/2048, therefore, it is unadvised to use the module
-- in such conditions.

constant clkdiv_inc : integer := OUT_FREQ*512;

signal   clkdiv_cnt : integer range 0 to CLK_FREQ + clkdiv_inc;
signal   clk_trig   : std_logic;


-- clk_counter is incremented at each clk_trig event, it is used to produce the different clocks required
-- by the Pmod
signal clk_counter  : unsigned(10 downto 2):="000000000";

-- The shift register for data
signal Shift_reg    : std_logic_vector(15 downto 0);


begin


-- generation of clk_trig
process(clk)
begin
    if rising_edge(clk) then
        if reset = '1' then
            clkdiv_cnt <=  0;
            clk_trig   <= '0';
        elsif clkdiv_cnt>CLK_FREQ then
            clkdiv_cnt <= clkdiv_cnt - CLK_FREQ + clkdiv_inc;
            clk_trig   <= '1';
        else
            clkdiv_cnt <= clkdiv_cnt + clkdiv_inc;
            clk_trig   <= '0';
        end if;
    end if;
end process;



-- generation of clocks
process(clk)
begin
   if rising_edge(clk) then
      if reset = '1' then
         clk_counter <= "000000000";
      elsif clk_trig = '1' then
         clk_counter <= clk_counter + 1;
      end if;
   end if;
end process;

MCLK <= not clk_counter(2); -- This clock is 256 times faster than LRCK 
LRCK <= clk_counter(10);    -- LRCK is the sampling frequency. when '1', sending left channel, when '0', sending right channel
SCLK <= clk_counter(5);     -- The clock for data bits, 32 times faster than LRCK.


-- This process controls the output shift register
-- there is no Reset management because correct values will be naturally set while process goes on
process(clk)
begin
   if rising_edge(clk) then
      if clk_counter(5 downto 2) = "1111" and clk_trig = '1' then     -- on the falling edge of SCLK
         if    clk_counter(10 downto 6) = "00000" then   -- 1st bit of the left channel to be output
            Shift_reg <= right;
         elsif clk_counter(10 downto 6) = "10000" then   -- 1st bit of the right channel to be output
            Shift_reg <= left;
         else                                            -- otherwise, we just shift the output register
            Shift_reg(15 downto 1) <= Shift_reg(14 downto 0);
         end if;
      end if;
   end if;
end process;

SDIN <= Shift_reg(15);
SYNC <= clk_trig when clk_counter = "100001111" else '0';

end Behavioral;

