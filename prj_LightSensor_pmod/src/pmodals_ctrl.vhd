----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    21:08:38 10/20/2015 
-- Design Name: 
-- Module Name:    pmodals_ctrl - Behavioral 
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

entity pmodals_ctrl is
    Port ( clk         : in  STD_LOGIC;
           reset       : in  STD_LOGIC;
           refresh     : in  STD_LOGIC;
           refreshing  : out  STD_LOGIC;
           data        : out  STD_LOGIC_VECTOR (7 downto 0);
           spi_ss      : out  STD_LOGIC;
           spi_miso    : in  STD_LOGIC;
           spi_sck     : out  STD_LOGIC);
end pmodals_ctrl;

architecture Behavioral of pmodals_ctrl is

type T_FSM is (idle, release_ss, first_dummy, data_bits, last_dummy, ss_up, fix_ss);

signal state    : t_FSM;

signal div_cnt   : integer range 0 to 15;
signal rem_bits  : integer range 0 to 7;
signal sck_int   : std_logic;
signal shift_reg : std_logic_vector(7 downto 0);

begin

spi_sck <= sck_int;
data    <= shift_reg;

process(clk)
begin
   if rising_edge(clk) then
      if reset = '1' then
         state <= idle;
      else
         case state is
            when idle        => if refresh='1' then
                                    state <= release_ss;
                                end if;
            when release_ss  => if div_cnt = 0 then
                                    state <= first_dummy;
                                end if;
            when first_dummy => if div_cnt = 0 and sck_int = '1' and rem_bits = 0 then 
                                    state <= data_bits;
                                end if;
            when data_bits   => if div_cnt = 0 and sck_int = '1' and rem_bits = 0 then 
                                    state <= last_dummy;
                                end if;
            when last_dummy  => if div_cnt = 0 and sck_int = '1' and rem_bits = 0 then 
                                    state <= ss_up;
                                end if;
            when ss_up       => if div_cnt = 0 then
                                    state <= fix_ss;
                                end if;
            when fix_ss      => if div_cnt = 0 then
                                    state <= idle;
                                end if;
         end case;
      end if;
   end if;
end process;


refreshing <= refresh when state = idle else '1';


process(clk)
begin
   if rising_edge(clk) then
      if state = data_bits and sck_int = '1' and div_cnt = 1 then
         shift_reg <= shift_reg(6 downto 0) & spi_miso;
      end if;
   end if;
end process;

process(clk)
begin
   if rising_edge(clk) then
      if reset = '1' then
         spi_ss <= '1';
      elsif state = release_ss then
         spi_ss <= '0';
      elsif state = fix_ss then
         spi_ss <= '1';
      end if;
   end if;
end process;

process(clk)
begin
   if rising_edge(clk) then
      if state = idle or state = release_ss or state = ss_up or state = fix_ss then
         sck_int <= '0';
      elsif div_cnt = 0 then
         sck_int <= not sck_int;
      end if;
   end if;   
end process;

process(clk)
begin
   if rising_edge(clk) then
      if state = idle then
         if refresh = '1' then
            div_cnt <= 15;
         end if;
      elsif div_cnt = 0 then
         if state = ss_up then
            div_cnt <= 15;
         else
            div_cnt <= 13;
         end if;
      else
         div_cnt <= div_cnt - 1;
      end if;
   end if;
end process;


process(clk)
begin
   if rising_edge(clk) then
      if state = idle then
         rem_bits <= 3;
      elsif (state = first_dummy or state = data_bits or state = last_dummy) and div_cnt = 0 and sck_int = '1' then
         if rem_bits = 0 then
            if state = first_dummy then 
               rem_bits <= 7;
            else
               rem_bits <= 3;
            end if;
         else
            rem_bits <= rem_bits - 1;
         end if;
      end if;
   end if;
end process;

end Behavioral;

