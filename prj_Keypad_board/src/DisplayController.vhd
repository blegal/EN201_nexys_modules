----------------------------------------------------------------------------------
-- Module Name:    7segment - Behavioral 
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
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;


entity DisplayController is
    Port ( 
			  --output from the Decoder
			  DispVal : in  STD_LOGIC_VECTOR (3 downto 0);
			  --controls the display digits
			  anode: out std_logic_vector(3 downto 0);
			  --controls which digit to display
           segOut : out  STD_LOGIC_VECTOR (6 downto 0)); 
end DisplayController;

architecture Behavioral of DisplayController is

--signal segOut_temp :  STD_LOGIC_VECTOR (6 downto 0); 

begin
	-- only display the leftmost digit
	anode<="0000";
			

	 with DispVal select
       segOut <=   "0000001" when "0000", --0
                   "1001111" when "0001", --1
                   "0010010" when "0010", --2
                   "0000110" when "0011", --3
                   "1001100" when "0100", --4
                   "0100100" when "0101", --5
                   "0100000" when "0110", --6
                   "0001111" when "0111", --7
                   "0000000" when "1000", --8
                   "0000100" when "1001", --9
                   "0001000" when "1010", --A
                   "1100000" when "1011", --B
                   "0110001" when "1100", --C
                   "1000010" when "1101", --D
                   "0110000" when "1110", --E
                   "0111000" when "1111", --F
                   "0111111" when others;	  
					  			
	
end Behavioral;

