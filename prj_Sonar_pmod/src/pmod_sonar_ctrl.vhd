----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    18:06:45 10/21/2015 
-- Design Name: 
-- Module Name:    pmod_SONAR_ctrl - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--
-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity pmod_SONAR_ctrl is
    Port ( clk      : in  STD_LOGIC;
           reset    : in  STD_LOGIC;
           rx       : out STD_LOGIC;
           pw       : in  STD_LOGIC;
           data_out : out STD_LOGIC_VECTOR (7 downto 0));
end pmod_SONAR_ctrl;

architecture Behavioral of pmod_SONAR_ctrl is

    signal value_cnt : integer range 0 to 255;
    signal sub_cnt   : integer range 0 to 16383;
    signal old_pw    : std_logic;
    
    constant max_sub_cnt : integer := 14705;

begin

rx <= '1';

process(clk)
begin
   if rising_edge(clk) then
      old_pw <= pw;
      if reset = '1' then
         data_out <= "00000000";
      elsif old_pw = '1' and pw = '0' then
         data_out <= std_logic_vector(to_unsigned(value_cnt, 8));
      end if;
   end if;
end process;

process(clk)
begin
   if rising_edge(clk) then
      if reset = '1' or pw = '0' or sub_cnt = max_sub_cnt then
         sub_cnt <= 0;
      else
         sub_cnt <= sub_cnt + 1;
      end if;
   end if;
end process;


process(clk)
begin
   if rising_edge(clk) then
      if reset = '1' or pw = '0' then
         value_cnt <= 0;
      elsif sub_cnt = max_sub_cnt and value_cnt < 255 then
         value_cnt <= value_cnt + 1;
      end if;
   end if;
end process;

end Behavioral;

