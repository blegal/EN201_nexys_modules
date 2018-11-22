----------------------------------------------------------------------------------

-- Module Name:    Decoder - Behavioral 
-- Project Name:  PmodKYPD
-- Target Devices: Nexys4
-- Tool versions: VIVADO
-- Description: 
--	This file defines a component Decoder for the demo project PmodKYPD. 
-- The Decoder scans each column by asserting a low to the pin corresponding to the column 
-- at 1KHz. After a column is asserted low, each row pin is checked. 
-- When a row pin is detected to be low, the key that was pressed could be determined.
--
-- C. JEGO 21/11/2018
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.numeric_std.all;

entity Decoder_cor is
    Port (
		  clk       : in  STD_LOGIC;
		  Raz       : in  STD_LOGIC;
          Row       : in  STD_LOGIC_VECTOR (3 downto 0);
	      Col       : out  STD_LOGIC_VECTOR (3 downto 0);
          DecodeOut : out  STD_LOGIC_VECTOR (3 downto 0)
		  );
end Decoder_cor;

architecture Behavioral of Decoder_cor is

signal sclk          : UNSIGNED(19 downto 0);
signal sig_DecodeOut : STD_LOGIC_VECTOR (3 downto 0);
signal sig_Col       : STD_LOGIC_VECTOR (3 downto 0); 

begin

 cpt : Process(clk, Raz)
    begin
	  if (Raz='1') then
	       sclk <= to_unsigned(0,20);
	  elsif (clk'event and clk = '1') then
	      if (sclk < to_unsigned(400001,20)) then
	       sclk <= sclk + 1;
		  else
		  sclk <= to_unsigned(0,20);
		  end if;
	  end if;
  end process;
  
val_Col: Process(clk, Raz)
    begin
	  if (Raz='1') then
	       sig_Col <= "0111";
	  elsif (clk'event and clk = '1') then
	      if (sclk < to_unsigned(100000,20))    then sig_Col<= "0111"; -- 1ms 
	      elsif (sclk < to_unsigned(200000,20)) then sig_Col<= "1011"; -- 2ms                     
	      elsif (sclk < to_unsigned(300000,20)) then sig_Col<= "1101"; -- 3ms
	      elsif (sclk < to_unsigned(400000,20)) then sig_Col<= "1110"; -- 4ms
	      else                                       sig_Col<= "0111";    
	      end if;
	 end if;
  end process;

Col <= sig_Col;		  
  

val_dec:	Process(clk, Raz)
    begin
	  if (Raz='1') then
	       sig_DecodeOut <= "0001";
	  elsif (clk'event and clk = '1') then
 
		 if (sclk = to_unsigned(100001,20) ) then if    Row = "0111" then sig_DecodeOut <= "0001"; --1
							                      elsif Row = "1011" then sig_DecodeOut <= "0100"; --4
				                                  elsif Row = "1101" then sig_DecodeOut <= "0111"; --7
				                                  elsif Row = "1110" then sig_DecodeOut <= "0000"; --0
				                                  else                    sig_DecodeOut <= sig_DecodeOut;
				                                  end if;
				                                  
		 elsif (sclk = to_unsigned(200001,20)) then if    Row = "0111" then sig_DecodeOut <= "0010"; --2
                                                    elsif Row = "1011" then sig_DecodeOut <= "0101"; --5
                                                    elsif Row = "1101" then sig_DecodeOut <= "1000"; --8
                                                    elsif Row = "1110" then sig_DecodeOut <= "1111"; --F
                                                    else                    sig_DecodeOut <= sig_DecodeOut;
                                                    end if;

		  elsif (sclk = to_unsigned(300001,20)) then if    Row = "0111" then sig_DecodeOut <= "0011"; --3
                                                     elsif Row = "1011" then sig_DecodeOut <= "0110"; --6
                                                     elsif Row = "1101" then sig_DecodeOut <= "1001"; --9
                                                     elsif Row = "1110" then sig_DecodeOut <= "1110"; --E
                                                     else                    sig_DecodeOut <= sig_DecodeOut;
                                                     end if;

		  elsif (sclk = to_unsigned(400001,20)) then if    Row = "0111" then sig_DecodeOut <= "1010"; --A
                                                     elsif Row = "1011" then sig_DecodeOut <= "1011"; --B
                                                     elsif Row = "1101" then sig_DecodeOut <= "1100"; --C
                                                     elsif Row = "1110" then sig_DecodeOut <= "1101"; --D
                                                     else                    sig_DecodeOut <= sig_DecodeOut;
                                                     end if;
          else sig_DecodeOut <= sig_DecodeOut;
          end if;
                                                            
	  end if;
    end process;
    
 DecodeOut <= sig_DecodeOut;   
						 
end Behavioral;

