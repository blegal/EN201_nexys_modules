----------------------------------------------------------------------------------
----------------------------------------------------------------------------
-- Author:  Albert Fazakas adapted from Alec Wyen and Mihaita Nagy
--          Copyright 2014 Digilent, Inc.
----------------------------------------------------------------------------
-- 
-- Create Date:    13:01:51 02/15/2013 
-- Design Name: 
-- Module Name:    Vga - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
-- Description: 
--       This module represents the Vga controller that creates the HSYNC and VSYNC signals
--    for the VGA screen and formats the 4-bit R, G and B signals to display various items
--    on the screen:
--       - A moving colorbar in the background
--       - A Digilent - Analog Devices logo for the Nexys4 board, the RGB data is provided 
--    by the LogoDisplay component. The logo bitmap is stored in the BRAM_1 Block RAM in .ngc format.
--       - The FPGA temperature on a 0..80C scale. Temperature data is taken from the XADC
--    component in the Artix-7 FPGA, provided by the upper level FPGAMonitor component and the RGB data is
--    provided by the Inst_XadcTempDisplay instance of the TempDisplay component.
--       - The Nexys4 Onboard ADT7420 Temperature Sensor temperature on a 0..80C scale. 
--    Temperature data is provided by the upper level TempSensorCtl component and the RGB data is
--    provided by the Inst_Adt7420TempDisplay instance of the TempDisplay component.
--       - The Nexys4 Onboard ADXL362 Accelerometer Temperature Sensor temperature on a 0..80C scale. 
--    Temperature data is provided by the upper level AccelerometerCtl component and the RGB data is
--    provided by the Inst_Adxl362TempDisplay instance of the TempDisplay component.
--       - The R, G and B data which is also sent to the Nexys4 onboard RGB Leds LD16 and LD17. The 
--    incomming RGB Led data is taken from the upper level RgbLed component and the formatted RGB data is provided
--    by the RGBLedDisplay component.
--       - The audio signal coming from the Nexys4 Onboard ADMP421 Omnidirectional Microphone. The formatted
--    RGB data is provided by the MicDisplay component.
--       - The X and Y acceleration in a form of a moving box and the acceleration magnitude determined by 
--    the SQRT (X^2 + Y^2 + Z^2) formula. The acceleration and magnitude data is provided by the upper level 
--    AccelerometerCtl component and the formatted RGB data is provided by the AccelDisplay component.
--       - The mouse cursor on the top on all of the items. The USB mouse should be connected to the Nexys4 board before 
--    the FPGA is configured. The mouse cursor data is provided by the upper level MouseCtl component and the 
--    formatted RGB data for the mouse cursor shape is provided by the MouseDisplay component.
--       - An overlay that displayed the frames and text for the displayed items described above. The overlay data is
--    stored in the overlay_bram Block RAM in the .ngc format and the data is provided by the OverlayCtl component.
--       The Vga controller holds the synchronization signal generation, the moving colorbar generation and the main
--    multiplexers for the outgoing R, G and B signals. Also the 108 MHz pixel clock (pxl_clk) generator is instantiated
--    inside the Vga controller.
--       The current resolution is 1280X1024 pixels, however, other resolutions can also be selected by 
--    commenting/uncommenting the corresponding VGA resolution constants. In the case when a different resolution
--    is selected, the pixel clock generator output frequency also has to be updated accordingly.
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.math_real.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity Vga is
    Port ( CLK_I : in  STD_LOGIC;
           -- VGA Output Signals
           VGA_HS_O          : out STD_LOGIC; -- HSYNC OUT
           VGA_VS_O          : out STD_LOGIC; -- VSYNC OUT
           VGA_RED_O         : out STD_LOGIC_VECTOR (3 downto 0); -- Red signal going to the VGA interface
           VGA_GREEN_O       : out STD_LOGIC_VECTOR (3 downto 0); -- Green signal going to the VGA interface
           VGA_BLUE_O        : out STD_LOGIC_VECTOR (3 downto 0); -- Blue signal going to the VGA interface
           -- Microphone
           MIC_M_DATA_I_1      : in  STD_LOGIC_VECTOR (15 downto 0);
           MIC_M_CLK_RISING_1  : IN  STD_LOGIC; -- Active when the data from the microphone is read
           MIC_M_DATA_I_2      : in  STD_LOGIC_VECTOR (15 downto 0);
           MIC_M_CLK_RISING_2  : IN  STD_LOGIC -- Active when the data from the microphone is read
           );
end Vga;

architecture Behavioral of Vga is

-------------------------------------------------------------------------

-- Component Declarations

-------------------------------------------------------------------------


   -- To generate the 108 MHz Pixel Clock
   -- needed for a resolution of 1280*1024 pixels
   COMPONENT clk_wiz_0
   PORT
    (-- Clock in ports
     CLK_IN1           : in std_logic;
     -- Clock out ports
     CLK_OUT1          : out std_logic;
     -- Status and control signals
     LOCKED            : out std_logic
    );
   END COMPONENT;

-------------------------------------------------------------

-- Constants for various VGA Resolutions

-------------------------------------------------------------

--***640x480@60Hz***--  
--constant FRAME_WIDTH : natural := 640;
--constant FRAME_HEIGHT : natural := 480;

--constant H_FP : natural := 16; --H front porch width (pixels)
--constant H_PW : natural := 96; --H sync pulse width (pixels)
--constant H_MAX : natural := 800; --H total period (pixels)
--
--constant V_FP : natural := 10; --V front porch width (lines)
--constant V_PW : natural := 2; --V sync pulse width (lines)
--constant V_MAX : natural := 525; --V total period (lines)

--constant H_POL : std_logic := '0';
--constant V_POL : std_logic := '0';

--***800x600@60Hz***--
--constant FRAME_WIDTH : natural := 800;
--constant FRAME_HEIGHT : natural := 600;
--
--constant H_FP : natural := 40; --H front porch width (pixels)
--constant H_PW : natural := 128; --H sync pulse width (pixels)
--constant H_MAX : natural := 1056; --H total period (pixels)
--
--constant V_FP : natural := 1; --V front porch width (lines)
--constant V_PW : natural := 4; --V sync pulse width (lines)
--constant V_MAX : natural := 628; --V total period (lines)
--
--constant H_POL : std_logic := '1';
--constant V_POL : std_logic := '1';

--***1280x1024@60Hz***--
constant FRAME_WIDTH  : natural := 1280;
constant FRAME_HEIGHT : natural := 1024;

constant H_FP  : natural  := 48; --H front porch width (pixels)
constant H_PW  : natural  := 112; --H sync pulse width (pixels)
constant H_MAX : natural := 1688; --H total period (pixels)

constant V_FP : natural  := 1; --V front porch width (lines)
constant V_PW : natural  := 3; --V sync pulse width (lines)
constant V_MAX : natural := 1066; --V total period (lines)

constant H_POL : std_logic := '1';
constant V_POL : std_logic := '1';

--***1920x1080@60Hz***--
--constant FRAME_WIDTH : natural := 1920;
--constant FRAME_HEIGHT : natural := 1080;
--
--constant H_FP : natural := 88; --H front porch width (pixels)
--constant H_PW : natural := 44; --H sync pulse width (pixels)
--constant H_MAX : natural := 2200; --H total period (pixels)
--
--constant V_FP : natural := 4; --V front porch width (lines)
--constant V_PW : natural := 5; --V sync pulse width (lines)
--constant V_MAX : natural := 1125; --V total period (lines)
--
--constant H_POL : std_logic := '1';
--constant V_POL : std_logic := '1';

------------------------------------------------------------------

-- Constants for setting the displayed logo size and coordinates

------------------------------------------------------------------
--constant SZ_LOGO_WIDTH 	    : natural := 335; -- Width of the logo frame
--constant SZ_LOGO_HEIGHT 	: natural := 280; -- Height of the logo frame

--constant FRM_LOGO_H_LOC 	: natural := 25; --  Starting horizontal location of the logo frame
--constant FRM_LOGO_V_LOC 	: natural := 176; -- Starting vertical location of the logo frame

---- Logo frame limits
--constant LOGO_LEFT 			: natural := FRM_LOGO_H_LOC - 1;
--constant LOGO_RIGHT 		: natural := FRM_LOGO_H_LOC + SZ_LOGO_WIDTH + 1;
--constant LOGO_TOP 			: natural := FRM_LOGO_V_LOC - 1;
--constant LOGO_BOTTOM 		: natural := FRM_LOGO_V_LOC + SZ_LOGO_HEIGHT + 1;

-----------------------------------------------------------------------------

-- Constants for setting size and location for the Microphone signal display

-----------------------------------------------------------------------------
constant SZ_MIC_WIDTH  		: natural := 1230; -- Width of the Microphone frame
constant SZ_MIC_HEIGHT 		: natural := 375;  -- Height of the Microphone frame

constant FRM_MIC_H_LOC 		: natural := 25; -- Microphone frame starting horizontal location
constant FRM_MIC_V_LOC 		: natural := 615; -- Microphone frame starting vertical location

-- Microphone display frame limits
constant MIC_LEFT			: natural := FRM_MIC_H_LOC - 1;
constant MIC_RIGHT			: natural := FRM_MIC_H_LOC + SZ_MIC_WIDTH + 1;
constant MIC_TOP			: natural := FRM_MIC_V_LOC - 1;
constant MIC_BOTTOM			: natural := FRM_MIC_V_LOC + SZ_MIC_HEIGHT + 1;

-----------------------------------------------------------------------------

-- Constants for setting size and location for the Microphone signal display

-----------------------------------------------------------------------------

constant FRM_MIC_H_LOC_2	: natural := 25; -- Microphone frame starting horizontal location
constant FRM_MIC_V_LOC_2	: natural := 34; -- Microphone frame starting vertical location

-- Microphone display frame limits
constant MIC_LEFT_2	        : natural := FRM_MIC_H_LOC_2 - 1;
constant MIC_RIGHT_2		: natural := FRM_MIC_H_LOC_2 + SZ_MIC_WIDTH + 1;
constant MIC_TOP_2			: natural := FRM_MIC_V_LOC_2 - 1;
constant MIC_BOTTOM_2		: natural := FRM_MIC_V_LOC_2 + SZ_MIC_HEIGHT + 1;

-------------------------------------------------------------------------

-- VGA Controller specific signals: Counters, Sync, R, G, B

-------------------------------------------------------------------------
-- Pixel clock, in this case 108 MHz
signal pxl_clk : std_logic;
-- The active signal is used to signal the active region of the screen (when not blank)
signal active  : std_logic;

-- Horizontal and Vertical counters
signal h_cntr_reg : std_logic_vector(11 downto 0) := (others =>'0');
signal v_cntr_reg : std_logic_vector(11 downto 0) := (others =>'0');

-- Pipe Horizontal and Vertical Counters
signal h_cntr_reg_dly   : std_logic_vector(11 downto 0) := (others => '0');
signal v_cntr_reg_dly   : std_logic_vector(11 downto 0) := (others => '0');

-- Horizontal and Vertical Sync
signal h_sync_reg : std_logic := not(H_POL);
signal v_sync_reg : std_logic := not(V_POL);
-- Pipe Horizontal and Vertical Sync
signal h_sync_reg_dly : std_logic := not(H_POL);
signal v_sync_reg_dly : std_logic :=  not(V_POL);

-- VGA R, G and B signals coming from the main multiplexers
signal vga_red_cmb   : std_logic_vector(3 downto 0);
signal vga_green_cmb : std_logic_vector(3 downto 0);
signal vga_blue_cmb  : std_logic_vector(3 downto 0);
--The main VGA R, G and B signals, validated by active
signal vga_red    : std_logic_vector(3 downto 0);
signal vga_green  : std_logic_vector(3 downto 0);
signal vga_blue   : std_logic_vector(3 downto 0);
-- Register VGA R, G and B signals
signal vga_red_reg   : std_logic_vector(3 downto 0) := (others =>'0');
signal vga_green_reg : std_logic_vector(3 downto 0) := (others =>'0');
signal vga_blue_reg  : std_logic_vector(3 downto 0) := (others =>'0');

-------------------------------------------------------------------------

-- Signals for registering the inputs

-------------------------------------------------------------------------
signal RGB_LED_RED_REG     : std_logic_vector (4 downto 0);
signal RGB_LED_BLUE_REG    : std_logic_vector (4 downto 0);
signal RGB_LED_GREEN_REG   : std_logic_vector (4 downto 0);

signal XADC_TEMP_VALUE_I_REG     : std_logic_vector (11 downto 0);
signal ADT7420_TEMP_VALUE_I_REG  : std_logic_vector (12 downto 0);
signal ADXL362_TEMP_VALUE_I_REG  : std_logic_vector (11 downto 0);

signal ACCEL_RADIUS_REG : STD_LOGIC_VECTOR (11 downto 0);
signal LEVEL_THRESH_REG : STD_LOGIC_VECTOR (11 downto 0);
signal ACL_X_IN_REG     : STD_LOGIC_VECTOR (8 downto 0);
signal ACL_Y_IN_REG     : STD_LOGIC_VECTOR (8 downto 0);
signal ACL_MAG_IN_REG   : STD_LOGIC_VECTOR (11 downto 0);

signal MIC_M_DATA_I_REG_1 : STD_LOGIC_VECTOR (15 downto 0);
signal MIC_M_DATA_I_REG_2 : STD_LOGIC_VECTOR (15 downto 0);

signal MOUSE_X_POS_REG  : std_logic_vector (11 downto 0);
signal MOUSE_Y_POS_REG  : std_logic_vector (11 downto 0);
signal MOUSE_LEFT_BUTTON_REG : std_logic;

-----------------------------------------------------------
-- Signals for generating the background (moving colorbar)
-----------------------------------------------------------
signal cntDyn				: integer range 0 to 2**28-1; -- counter for generating the colorbar
signal intHcnt				: integer range 0 to H_MAX - 1;
signal intVcnt				: integer range 0 to V_MAX - 1;
-- Colorbar red, greeen and blue signals
signal bg_red 				: std_logic_vector(3 downto 0);
signal bg_blue 			: std_logic_vector(3 downto 0);
signal bg_green 			: std_logic_vector(3 downto 0);
-- Pipe the colorbar red, green and blue signals
signal bg_red_dly			: std_logic_vector(3 downto 0) := (others => '0');
signal bg_green_dly		: std_logic_vector(3 downto 0) := (others => '0');
signal bg_blue_dly		: std_logic_vector(3 downto 0) := (others => '0');


-------------------------------------------------------------------------

-- Interconnection signals for the displaying components

-------------------------------------------------------------------------

-- Microphone data display signals
signal mic_red 			: std_logic_vector(3 downto 0);
signal mic_blue 		: std_logic_vector(3 downto 0);
signal mic_green 		: std_logic_vector(3 downto 0);

signal mic_red_2 		: std_logic_vector(3 downto 0);
signal mic_blue_2 		: std_logic_vector(3 downto 0);
signal mic_green_2 		: std_logic_vector(3 downto 0);

---------------------------------------------------------------------------------

-- Pipe all of the interconnection signals coming from the displaying components

---------------------------------------------------------------------------------

-- Registered Microphone data display signals
signal mic_red_dly 			: std_logic_vector(3 downto 0);
signal mic_blue_dly 		: std_logic_vector(3 downto 0);
signal mic_green_dly 		: std_logic_vector(3 downto 0);

signal mic_red_dly_2		: std_logic_vector(3 downto 0);
signal mic_blue_dly_2 		: std_logic_vector(3 downto 0);
signal mic_green_dly_2 		: std_logic_vector(3 downto 0);

begin
  
------------------------------------

-- Generate the 108 MHz pixel clock 

------------------------------------
   Inst_PxlClkGen: clk_wiz_0
   port map
    (-- Clock in ports
     CLK_IN1   => CLK_I,
     -- Clock out ports
     CLK_OUT1  => pxl_clk,
     -- Status and control signals
     LOCKED   => open
    );

---------------------------------------------------------------

-- Generate Horizontal, Vertical counters and the Sync signals

---------------------------------------------------------------
  -- Horizontal counter
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      if (h_cntr_reg = (H_MAX - 1)) then
        h_cntr_reg <= (others =>'0');
      else
        h_cntr_reg <= h_cntr_reg + 1;
      end if;
    end if;
  end process;

  -- Vertical counter
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      if ((h_cntr_reg = (H_MAX - 1)) and (v_cntr_reg = (V_MAX - 1))) then
        v_cntr_reg <= (others =>'0');
      elsif (h_cntr_reg = (H_MAX - 1)) then
        v_cntr_reg <= v_cntr_reg + 1;
      end if;
    end if;
  end process;

  -- Horizontal sync
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      if (h_cntr_reg >= (H_FP + FRAME_WIDTH - 1)) and (h_cntr_reg < (H_FP + FRAME_WIDTH + H_PW - 1)) then
        h_sync_reg <= H_POL;
      else
        h_sync_reg <= not(H_POL);
      end if;
    end if;
  end process;

  -- Vertical sync
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
      if (v_cntr_reg >= (V_FP + FRAME_HEIGHT - 1)) and (v_cntr_reg < (V_FP + FRAME_HEIGHT + V_PW - 1)) then
        v_sync_reg <= V_POL;
      else
        v_sync_reg <= not(V_POL);
      end if;
    end if;
  end process;
  
--------------------

-- The active 

--------------------  
  -- active signal
  active <= '1' when h_cntr_reg_dly < FRAME_WIDTH and v_cntr_reg_dly < FRAME_HEIGHT
            else '0';

--------------------

-- Register Inputs

--------------------
register_inputs: process (pxl_clk, v_sync_reg)
  begin
    if (rising_edge(pxl_clk)) then
      -- Incoming Microphone data rate is faster than VSYNC, therefore is registered on the pixel clock
      MIC_M_DATA_I_REG_1 <= MIC_M_DATA_I_1;
      MIC_M_DATA_I_REG_2 <= MIC_M_DATA_I_2;
    end if;
end process register_inputs;

--------------------------------------

-- Microphone signal display instance

--------------------------------------

	Inst_MicDisplay: ENTITY work.MicDisplay 
	GENERIC MAP(
	  X_WIDTH 	        => SZ_MIC_WIDTH,
      Y_HEIGHT 		    => SZ_MIC_HEIGHT,
      X_START 			=> FRM_MIC_H_LOC,
      Y_START 			=> FRM_MIC_V_LOC,
      PXLCLK_FREQ_HZ    => 108000000,
      H_MAX             => H_MAX,
      SAMPLE_RATE_DIV   => 4096,
      BG_COLOR 		    => x"FFF",
      ACTIVE_COLOR	    => x"008"
	)
	PORT MAP(
		CLK_I             => pxl_clk,
      SYSCLK              => CLK_I,
		MIC_M_DATA_I      => MIC_M_DATA_I_REG_1,
      MIC_M_CLK_RISING    => MIC_M_CLK_RISING_1,
		H_COUNT_I         => h_cntr_reg,
		V_COUNT_I         => v_cntr_reg,
		RED_O             => mic_red,
		GREEN_O           => mic_green,
		BLUE_O            => mic_blue
      );

	Inst_MicDisplay2: ENTITY work.MicDisplay 
	GENERIC MAP(
	  X_WIDTH 	        => SZ_MIC_WIDTH,
      Y_HEIGHT 		    => SZ_MIC_HEIGHT,
      X_START 			=> FRM_MIC_H_LOC_2,
      Y_START 			=> FRM_MIC_V_LOC_2,
      PXLCLK_FREQ_HZ    => 108000000,
      H_MAX             => H_MAX,
      SAMPLE_RATE_DIV   => 4096
      --BG_COLOR 		    => x"FFF",
      --ACTIVE_COLOR	    => x"008"
	)
	PORT MAP(
		CLK_I           => pxl_clk,
      SYSCLK            => CLK_I,
		MIC_M_DATA_I    => MIC_M_DATA_I_REG_2,
      MIC_M_CLK_RISING  => MIC_M_CLK_RISING_2,
		H_COUNT_I       => h_cntr_reg,
		V_COUNT_I       => v_cntr_reg,
		RED_O           => mic_red_2,
		GREEN_O         => mic_green_2,
		BLUE_O          => mic_blue_2
      );

              
----------------------------------

-- Accelerometer display instance

----------------------------------
--   Inst_AccelDisplay: AccelDisplay 
--   GENERIC MAP
--   (
--      X_XY_WIDTH		=> SZ_ACL_XY_WIDTH, -- Width of the Accelerometer frame X-Y region
--      X_MAG_WIDTH    => SZ_ACL_MAG_WIDTH, -- Width of the Accelerometer frame Magnitude region
--      Y_HEIGHT 		=> SZ_ACL_HEIGHT, -- Height of the Accelerometer frame
--      X_START 			=> FRM_ACL_H_LOC, -- Accelerometer frame X-Y region starting horizontal location
--      Y_START 			=> FRM_ACL_V_LOC, -- Accelerometer frame starting vertical location
--      BG_COLOR 		=> x"FFF", -- White
--      ACTIVE_COLOR 	=> x"0F0", -- Green
--      WARNING_COLOR 	=> x"F00" -- Red
--    )
--    PORT MAP
--    (
--      CLK_I => pxl_clk,
--      ACCEL_X_I => ACL_X_IN_REG,
--      ACCEL_Y_I => ACL_Y_IN_REG,
--      ACCEL_MAG_I => ACL_MAG_IN_REG(8 DOWNTO 0), -- only 9 bits are taken into account, data is scaled between 0-500
--      H_COUNT_I => h_cntr_reg,
--      V_COUNT_I => v_cntr_reg,
--      ACCEL_RADIUS => ACCEL_RADIUS_REG,
--      LEVEL_THRESH => LEVEL_THRESH_REG,
--      RED_O => acl_red,
--      BLUE_O => acl_blue,
--      GREEN_O => acl_green
--	 );
    

----------------------------------

-- Mouse Cursor display instance

----------------------------------
--   Inst_MouseDisplay: MouseDisplay
--   PORT MAP 
--   (
--      pixel_clk   => pxl_clk,
--      xpos        => MOUSE_X_POS_REG, 
--      ypos        => MOUSE_Y_POS_REG,
--      hcount      => h_cntr_reg,
--      vcount      => v_cntr_reg,
--      enable_mouse_display_out  => enable_mouse_display,
--      red_out     => mouse_cursor_red,
--      green_out   => mouse_cursor_green,
--      blue_out    => mouse_cursor_blue
--   );

----------------------------------

-- Overlay display instance

----------------------------------
--    	Inst_OverlayCtrl: OverlayCtl 
--      PORT MAP
--      (
--		CLK_I       => pxl_clk,
--		VSYNC_I     => v_sync_reg,
--		ACTIVE_I    => active,
--		OVERLAY_O   => overlay_en
--      );
  
  
---------------------------------------

-- Generate moving colorbar background

---------------------------------------

	process(pxl_clk)
	begin
		if(rising_edge(pxl_clk)) then
			cntdyn <= cntdyn + 1;
		end if;
	end process;
   
  	intHcnt <= conv_integer(h_cntr_reg);
	intVcnt <= conv_integer(v_cntr_reg);
	
	bg_red   <= (OTHERS => '0');--conv_std_logic_vector((-intvcnt - inthcnt - cntDyn/2**20),8)(7 downto 4);
	bg_green <= (OTHERS => '0');--conv_std_logic_vector((inthcnt - cntDyn/2**20),8)(7 downto 4);
	bg_blue  <= (OTHERS => '0');--conv_std_logic_vector((intvcnt - cntDyn/2**20),8)(7 downto 4);
   
---------------------------------------------------------------------------------------------------

-- Register Outputs coming from the displaying components and the horizontal and vertical counters

---------------------------------------------------------------------------------------------------
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then
   
      
	   mic_red_dly			<= mic_red;
		mic_green_dly		<= mic_green;
		mic_blue_dly		<= mic_blue;

	   mic_red_dly_2			<= mic_red_2;
		mic_green_dly_2		<= mic_green_2;
		mic_blue_dly_2		<= mic_blue_2;

      bg_red_dly			<= bg_red;
		bg_green_dly		<= bg_green;
		bg_blue_dly			<= bg_blue;

      
      h_cntr_reg_dly <= h_cntr_reg;
		v_cntr_reg_dly <= v_cntr_reg;
      
      
    end if;
  end process;


-------------------------------------------------------------

-- Main Multiplexers for the VGA Red, Green and Blue signals

-------------------------------------------------------------
----------
-- Red
----------

  vga_red <=   -- Mouse_cursor_display is on the top of others
               mic_red_dly when h_cntr_reg_dly > MIC_LEFT and h_cntr_reg_dly < MIC_RIGHT 
                            and v_cntr_reg_dly > MIC_TOP and v_cntr_reg_dly < MIC_BOTTOM
           else
               mic_red_dly_2 when h_cntr_reg_dly > MIC_LEFT_2 and h_cntr_reg_dly < MIC_RIGHT_2 
                            and v_cntr_reg_dly > MIC_TOP_2 and v_cntr_reg_dly < MIC_BOTTOM_2
           else
               bg_red_dly;

-----------
-- Green
-----------

  vga_green <= -- Mouse_cursor_display is on the top of others
               mic_green_dly when h_cntr_reg_dly > MIC_LEFT and h_cntr_reg_dly < MIC_RIGHT 
                 and v_cntr_reg_dly > MIC_TOP and v_cntr_reg_dly < MIC_BOTTOM
           else
               mic_green_dly_2 when h_cntr_reg_dly > MIC_LEFT_2 and h_cntr_reg_dly < MIC_RIGHT_2 
                 and v_cntr_reg_dly > MIC_TOP_2 and v_cntr_reg_dly < MIC_BOTTOM_2
           else
               -- Colorbar will be on the backround
               bg_green_dly;

-----------
-- Blue
-----------

  vga_blue <=  -- Mouse_cursor_display is on the top of others
               mic_blue_dly when h_cntr_reg_dly > MIC_LEFT and h_cntr_reg_dly < MIC_RIGHT 
                             and v_cntr_reg_dly > MIC_TOP and v_cntr_reg_dly < MIC_BOTTOM
           else
               mic_blue_dly_2 when h_cntr_reg_dly > MIC_LEFT_2 and h_cntr_reg_dly < MIC_RIGHT_2 
                             and v_cntr_reg_dly > MIC_TOP_2 and v_cntr_reg_dly < MIC_BOTTOM_2
           else
               -- Colorbar will be on the backround
               bg_blue_dly;
                

------------------------------------------------------------
-- Turn Off VGA RBG Signals if outside of the active screen
-- Make a 4-bit AND logic with the R, G and B signals
------------------------------------------------------------
 vga_red_cmb   <= (active & active & active & active) and vga_red;
 vga_green_cmb <= (active & active & active & active) and vga_green;
 vga_blue_cmb  <= (active & active & active & active) and vga_blue;
 

 -- Register Outputs
  process (pxl_clk)
  begin
    if (rising_edge(pxl_clk)) then

      v_sync_reg_dly <= v_sync_reg;
      h_sync_reg_dly <= h_sync_reg;
      vga_red_reg    <= vga_red_cmb;
      vga_green_reg  <= vga_green_cmb;
      vga_blue_reg   <= vga_blue_cmb;      
    end if;
  end process;

  -- Assign outputs
  VGA_HS_O     <= h_sync_reg_dly;
  VGA_VS_O     <= v_sync_reg_dly;
  VGA_RED_O    <= vga_red_reg;
  VGA_GREEN_O  <= vga_green_reg;
  VGA_BLUE_O   <= vga_blue_reg;

end Behavioral;
