----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 23.11.2016 10:17:37
-- Design Name: 
-- Module Name: Affichage - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
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
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity Affichage is
    Port ( clk : in STD_LOGIC;
           reset : in STD_LOGIC;
           FeuVoitureP : in STD_LOGIC_VECTOR (2 downto 0); -- 100 vert / 010 orange /001 rouge
           FeuVoitureS : in STD_LOGIC_VECTOR (2 downto 0);
           FeuPietonP : in STD_LOGIC; --rouge 0 / vert 1
           FeuPietonS : in STD_LOGIC;

           FeuRP : out STD_LOGIC;
           FeuOP : out STD_LOGIC;
           FeuVP : out STD_LOGIC;
           FeuRS : out STD_LOGIC;
           FeuOS : out STD_LOGIC;
           FeuVS : out STD_LOGIC;
           PietonRP : out STD_LOGIC;
           PietonVP : out STD_LOGIC;
           PietonRS : out STD_LOGIC;
           PietonVS : out STD_LOGIC
         );
          
end Affichage;


architecture Behavioral of Affichage is

--signal FeuP: out STD_LOGIC_VECTOR (2 downto 0);
--signal FeuS: out STD_LOGIC_VECTOR (2 downto 0);
--signal PietonP: out STD_LOGIC_VECTOR (1 downto 0);
--signal PietonS: out STD_LOGIC_VECTOR (1 downto 0);


begin



end Behavioral;
