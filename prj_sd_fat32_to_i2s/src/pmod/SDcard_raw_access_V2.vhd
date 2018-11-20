----------------------------------------------------------------------------------------------------
-- SDcard_raw_access_v2
--    Version V2.0.2 (2017/07/28)
--    (c)2017 Y. BORNAT - Bordeaux INP / ENSEIRB-MATMECA
--    This module manages data transfers to and from SD/SDHC card (SDXC might work, not tested)
----------------------------------------------------------------------------------------------------
-- First working version.
-- TODO list :
--    Complete main FSM to provide :
--              - timeout management in each state/command
--              - automatic retry after CRC fail
--    Optimisation improvements
--              - merge FSMs for receive and send commands
--              - switch some signals to async to reduce state change latency
--    simplify CRC checking circuitry for incoming data (also saves HW resources)
--    more accurate buffer-to-output timings ?
--    on reset, should also take time to reset the card... (usefull ?)
--
-- known bugs :
--    - The module seems to provide a different behavior when inserting an SDcard for the first time after
--      reset and after the other insertions...
--    - to be fully compliant with specs, first instructions should be performed at 400kHz, but they are
--      performed at 25MHz. (MMC compatibility ?)
--
-------------------------------------------------------------------------------------------------------
-- How to use This module :
-------------------------------------------------------------------------------------------------------
-- SDcards are organized as arrays of 512byte blocks. So the module transfers full blocks to and from
-- the SDcard. The signals loc_buffer, loc_buffer, data_write, data_in and data_out make it possible 
-- to read and write to buffers as memory. loc_buffer is the buffer number.

-- The prep_block input controls a sequence that improves performance when using multiple blocks
-- operation. When set, the number of blocks to use for the next multiple operation is read in
-- SD_block, the busy flag remains asserted for a few µs. If the following operation is not a
-- multiple block operation, this commands is just useless. If more blocks are used for the
-- following operation, performance will decrease for the additionnal blocks. If less blocks are
-- used for a multiple write operation, the blocks prepared but not written might or might not
-- be erased. This command does not improve performance while reading from SDSC cards.
-- 
-- in the following diagrams, all signals that are not shown are tied to 0.
--
-- Loading a buffer to a block in the SDcard:
---------------------------------------------
--                   ____      ___
--   clk           _/    \____/           ...
-- 
--   TR_buffer     --< buffer # >-------  ...
--
--   SD_block      --< block #  >-------  ...
--                    __________
--   write_block   __/          \_______  ...
--                               _______       ___  module ready for new operation
--   busy          _____________/         ...     \__________
--
--                                                 
-- retreiving a block from the SDcard to a buffer:
--------------------------------------------------
--                   ____      ___
--   clk           _/    \____/           ...
-- 
--   TR_buffer     --< buffer # >-------  ...
--
--   SD_block      --< block #  >-------  ...
--                   __________
--   read_block    __/          \_______  ...
--                               _______       ___  module ready for new operation
--   busy          _____________/         ...     \__________
--
--
--
-- multiple read/write operations
---------------------------------
--                   ____      ___                ____      ____      ____               ____      ____      ____
--   clk           _/    \____/           ...  __/    \____/    \____/    \____  ...  __/    \____/    \____/    \____  ...  
-- 
--   TR_buffer     --< buffer #>--------  ...  -------------< buffer #>--------  ...  -------------< buffer #>--------  ...  
--
--   SD_block      --< block # >--------  ...  --------------------------------  ...  --------------------------------  ...  
--                    __________                             _________                              _________
--   xxxx_block    __/          \_______  ...  _____________/         \________  ...  _____________/         \________  ...  
--                    __________________  ...  ________________________________       _______________________
--   multiple      __/                                                           ...                         \________  ...  
--                               _______       ___                     ________       ___                     ________       ___  module ready for new operation
--   busy          _____________/         ...     \___________________/          ...     \___________________/          ...     \__________
--
--
--                   First transfer                    incremental transfer                last transfer
--
-- erasing a block on the SDcard
--------------------------------
--                   ____      ___
--   clk           _/    \____/           ...
--
--   SD_block      --< block #  >-------  ...
--                    __________
--   erase_block   __/          \_______  ...
--                               _______       ___  module ready for new operation
--   busy          _____________/         ...     \__________
--
--
-- erasing multiple blocks on the SDcard
----------------------------------------
--                   ____      ___                ____      ____      ____               ____     
--   clk           _/    \____/           ...  __/    \____/    \____/    \____  ...  __/    \____
--
--   SD_block      --< first # >--------  ...  -------------< last #  >-------- 
--                    __________                             _________
--   erase_block   __/          \_______  ...  _____________/         \________  ...  ____________
--                    __________________       _______________________
--   multiple      __/                    ...                         \________  ...  
--                               _______       ___                     ________       ___  module ready for new operation
--   busy          _____________/         ...     \___________________/          ...     \________
--
--
-- The 'blocks' output give the amount of available blocks in the SDcard
--
--    Although the technical limitation for WORD_SIZE_PW2 is 8 (256bytes or 2kbit words),
--    the value is limited to 3 (8 bytes or 64bits) to limit resource usage
--
--
----------------------------------------------------------------------------------
-- History
----------------------------------------------------------------------------------
-- V2.1 (2017/12/05) yb
--    - code updates to fix regressions from ISE to Vivado
--      - added the VIVADO_SYNTH generic input to identify which code to use...
-- V2.0.2 (2017/07/28) yb
--    - minor comment additions
--    - changed endianness management to simplify code
--    - entered coherent power-on values to fit reset state
--    - improved behavior on reset
--    - Fixed : module was using SD_CD without synchronizing it
--    - Fixed : bytes 0 to 511 present in addresses 1 to 512 when storing 8bit DATA at 50MHz with frequ<200MHz
--    - Fixed : write single block command sent twice when bursting accesses, resulting in resp_timeout
--    - Fixed : SDSC CRC error
--    - Fixed : System could stall if CMD23 not available on SDHC cards
--    - added HIGH_SPEED_IF generic to get a reliable behavior
--    - added optionnal input SD_WP (for write protection) to inhibit write and erase on protected cards.
-- V2.0.1 (2017/03/28)
--    - Fixed : SDHC cards were not detected (used in SDSC mode)
--    - added frequency switch to achieve 50MHz transfers
--    - added Timeout error reporting
--    - added multiple block command preparation (prep_block input) to improve performance
--    - removed generic SCK_HALF_PERIOD witch is now computed from CLK_FREQ_HZ
-- V2.0 (2017/02/24)
--    - initial release
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


entity SDcard_raw_access_V2 is
    Generic(CLK_FREQ_HZ         : integer := 100000000;           -- the frequency of the clock expressed in Hz
            VIVADO_SYNTH        : boolean := True;                -- select the correct part of code to use to get a proper RAM implementation in vivado
            HIGH_SPEED_IF       : boolean := False;               -- When True, runs SDcard bus @50MHz (might not be reliable)
            WORD_SIZE_PW2       : integer range 0 to 3 := 0;      -- codes the data size of interface : it is 2^WORD_SIZE_PW2 bytes
            BUFFER_NUM_PW2      : integer range 1 to 8 := 2;      -- codes the number of 512B data buffers for transfers, actual value will be 2^BUFFER_NUM_PW2
            PARAMETER_BUFFERING : boolean := True;                -- if true, the module keeps its own copy of input parameter SD_block
                                                                  --     setting this generic to false may save slice FFs, but SD_block
                                                                  --     input MUST NOT change as long as read_block, write_block or busy are set.
            FILENAME            : string  := "Unused";            -- Unused, left for compatibility with simulation model
            LITTLE_ENDIAN       : boolean := True);               -- when multiple bytes per word, tells which byte is stored at lower address
                                                                  --     little endian (True) mean least significant byte first

    Port ( clk          : in    STD_LOGIC;
           reset        : in    STD_LOGIC;                                          -- must be valid two clock cycles.
           
           SD_block     : in    STD_LOGIC_VECTOR (31 downto 0);                     -- the block on which to perform the operation
           TR_buffer    : in    STD_LOGIC_VECTOR (BUFFER_NUM_PW2-1 downto 0);       -- the buffer on which to perform the operation
           read_block   : in    STD_LOGIC;                                          -- read operation
           write_block  : in    STD_LOGIC;                                          -- write operation
           erase_block  : in    STD_LOGIC := '0';                                   -- erase operation (different than write zeros, improves further reading)
           multiple     : in    STD_LOGIC := '0';                                   -- enables the multiple block operations
           prep_block   : in    STD_LOGIC := '0';                                   -- indicates the number of blocks to prepare for next operation
           busy         : out   STD_LOGIC;                                          -- module is busy
           err_code     : out   STD_LOGIC_VECTOR ( 3 downto 0);                     -- the error numer
           blocks       : out   STD_LOGIC_VECTOR (31 downto 0);                     -- number of available blocks
           
           loc_buffer   : in    STD_LOGIC_VECTOR (BUFFER_NUM_PW2-1 downto 0);       -- buffer for local access
           address      : in    STD_LOGIC_VECTOR (8-WORD_SIZE_PW2  downto 0);       -- data address in the buffer
           data_write   : in    STD_LOGIC;                                          -- data write to buffer
           data_in      : in    STD_LOGIC_VECTOR (8*(2**WORD_SIZE_PW2)-1 downto 0); -- data to be written to buffer
           data_out     : out   STD_LOGIC_VECTOR (8*(2**WORD_SIZE_PW2)-1 downto 0); -- data read from buffer

           SD_DAT       : inout STD_LOGIC_VECTOR ( 3 downto 0);                     -- DATA line for SDcard access
           SD_CD        : in    STD_LOGIC;                                          -- SDcard detected (active low)
           SD_WP        : in    STD_LOGIC := '0';                                   -- (optional) SDcard write protect switch
           SD_CLK       : out   STD_LOGIC;                                          -- Communication clock
           SD_CMD       : inout STD_LOGIC);                                         -- Command line

end SDcard_raw_access_V2;

architecture Behavioral of SDcard_raw_access_V2 is
   -- general constants
   constant TARG_FREQ_SLOW        : integer := 25000000; -- slow clock target frequency (25MHz)
   constant TARG_FREQ_FAST        : integer := 50000000; -- fast clock target frequency (50MHz)

   -- dimension constants
   constant WORD_SIZE_BITS        : integer := 8*(2**WORD_SIZE_PW2); -- the data size of words in buffers expressed in bits
   constant WORD_SIZE_BYTES       : integer :=    2**WORD_SIZE_PW2;  -- the data size of words expressed in bytes
   
   -- timing constants for clock generation
   constant SCK_HALF_PERIOD_SLOW  : integer := (CLK_FREQ_HZ-1)/(2*TARG_FREQ_SLOW);                                  -- periods of SDcard clock are 2*(SCK_HALF_PERIOD+1) clock cycles
   constant SCK_HALF_PERIOD_FAST  : integer := (CLK_FREQ_HZ-1)/(2*TARG_FREQ_FAST);                                  -- periods of SDcard clock are 2*(SCK_HALF_PERIOD+1) clock cycles
                                                                                                                    -- the final slow clock period will be (SCK_HALF_LOW_FAST + SCK_HALF_HIGH_FAST + 2) clock cycles
   
   -- timing constants for communication timeouts
   constant INIT_POWER_ON_TIMEOUT : integer := CLK_FREQ_HZ/500;                                  -- response timeout is 2ms. (must be <= INSTR_RESP_TIMEOUT)
   constant INIT_CMD0_TIMEOUT     : integer := (SCK_HALF_PERIOD_SLOW*2 + 2)*60;  -- 60 clock edges
   constant INIT_FREQ_SW_TIMEOUT  : integer := (SCK_HALF_PERIOD_SLOW*2 + 2)*16;  -- 16 clock edges
   constant INSTR_RESP_TIMEOUT    : integer := CLK_FREQ_HZ/10;       -- response timeout is 100ms.



   constant HALF_PER_LOWCLK       : integer := CLK_FREQ_HZ/800000;   -- first instructions should be low frequency 400kHz
                                                                     -- FIXME : not used yet

   -- error codes
   constant NO_ERROR              : std_logic_vector(3 downto 0) := "0000";
   
   constant WRITE_PROTECTED       : std_logic_vector(3 downto 0) := "0001";   -- write protection mechanical switch is enabled on the card

   constant RETRY_AFTER_BAD_CRC   : std_logic_vector(3 downto 0) := "0100";
   constant MULT_OP_CANCELLED     : std_logic_vector(3 downto 0) := "0101";
   constant CMD23_TIMEOUT         : std_logic_vector(3 downto 0) := "0110";
   
   constant NO_CARD               : std_logic_vector(3 downto 0) := "1000";
   constant INVALID_CARD          : std_logic_vector(3 downto 0) := "1001";
   constant INVALID_DATA_BLOCK    : std_logic_vector(3 downto 0) := "1010";
   constant WRITE_ERROR_FROM_CARD : std_logic_vector(3 downto 0) := "1011";
   constant INCONSISTENT_CSD      : std_logic_vector(3 downto 0) := "1100";
   constant TOO_MANY_CRC_RETRIES  : std_logic_vector(3 downto 0) := "1101";
   constant CARD_RESP_TIMEOUT     : std_logic_vector(3 downto 0) := "1110";
   constant NO_W_ON_WP_CARD       : std_logic_vector(3 downto 0) := "1111";   -- a write attempt has been performed on a protected card

   -----------------------------------------------------
   -- Signals related to user interface
   -----------------------------------------------------
   signal longreset            : boolean;                                        -- maintains reset as long as needed by module
   signal SD_block_local_cpy   : std_logic_vector (31 downto 0);                 -- local copy of SD_block
   signal busy_i               : std_logic;                                      -- local readable copy of busy output

   -- the memory that contains buffers
   type t_buffers is array(0 to 512*(2**BUFFER_NUM_PW2)/WORD_SIZE_BYTES - 1) of std_logic_vector(WORD_SIZE_BITS-1 downto 0);
   signal buffers : t_buffers;
   signal user_addr : std_logic_vector(8 + BUFFER_NUM_PW2 - WORD_SIZE_PW2 downto 0);

   -----------------------------------------------------
   -- Main FSM of the module
   -----------------------------------------------------
   type t_module_fsm is (wait_pw_on,         -- wait for SDcard to power on (1ms min)
                         wait_72clocks,      -- send the 72 clock edges (9 bytes) before asserting CS_n
                         init_cmd0,          -- card initialization
                         wait_icmd0_resp,    -- waits for response from SDcard to cmd0
                         init_cmd8,          -- voltage checking
                         wait_icmd8_resp,    -- waits for response from SDcard to cmd8
                         init_cmd55,         -- prepare next command as acmd
                         wait_icmd55_resp,   -- waits for response from SDcard to cmd55
                         init_acmd41,        -- initialization finished ?
                         wait_iacmd41_resp,  -- waits for response from SDcard to acmd41
                         init_cmd2,          -- asks for card identification 
                         wait_icmd2_resp,    -- waits for response from SDcard to cmd2
                         init_cmd3,          -- asks for short ID
                         wait_icmd3_resp,    -- waits for response from SDcard to cmd3
                         init_cmd9,          -- get info on the card (size)
                         wait_icmd9_resp,    -- waits for response from SDcard to cmd9
                         init_cmd7,          -- switch to transfer state
                         wait_icmd7_resp,    -- waits for response from SDcard to cmd7
                         init_cmd55b,        -- prepare next command as acmd
                         wait_icmd55b_resp,  -- waits for response from SDcard to cmd55
                         init_acmd42,        -- disable pullup resistor on data3
                         wait_iacmd42_resp,  -- waits for response from SDcard to acmd42
                         init_cmd6r,         -- the read of switch function capacity, this command may not be accepted by very old SDcard
                         wait_icmd6r_resp,   -- waits for the response from SDcard to cmd6
                         wait_icmd6r_status, -- reads back the data packet from cmd6
                         init_cmd6w,         -- the switch function to set high speed mode and allow higher consumption/performance, this command may not be accepted by very old SDcard
                         wait_icmd6w_resp,   -- waits for the response from SDcard to cmd6
                         wait_icmd6w_status, -- reads back the data packet from cmd6
                         wait_speed_trans,   -- waits a bit for the card to switch freq
                         init_cmd55c,        -- prepare next command as acmd
                         wait_icmd55c_resp,  -- waits for response from SDcard to cmd55
                         init_acmd6,         -- switch to 4line data transfer, this command should happen before CMD6, but we can't do UHS1 mode, so it doesn't mater
                         wait_iacmd6_resp,   -- waits for response from SDcard to acmd6
                         ready,              -- waiting for commands
                         read_cmd17,         -- in the process of reading a block
                         wait_rcmd17_resp,   -- waits for response from SDcard to cmd17
                         wait_rcmd17_dblk,   -- waits until data block has arrived after cmd17
                         readm_cmd18,        -- read multiple blocks
                         wait_rcmd18_resp,   -- waits for response from SDcard to cmd18
                         wait_rcmd18_dblk,   -- waits until data block has arrived after cmd18
                         readm_ready,        -- in a multiple block read, waiting for orders
                         stop_mread_cmd12,   -- send the command to stop transfer
                         wait_rmread_cmd12,  -- waits for cmd12 response
                         erase_mult_cmd32,   -- start sending first erase command for multiple blocks
                         wait_mecmd32_resp,  -- waits for response from SDcard in multiple blocks erase
                         erase_m_ready,      -- waits for end block command in multiple blocks
                         erase_cmd32,        -- start sending first erase command
                         wait_ecmd32_resp,   -- waits for response from SDcard
                         erase_cmd33,        -- start sending last block erase command
                         wait_ecmd33_resp,   -- waits for response from SDcard
                         erase_cmd38,        -- start sending actual erase command
                         wait_ecmd38_resp,   -- waits for response from SDcard
                         write_cmd24,        -- start sending write block command
                         wait_wcmd24_resp,   -- waits for response from SDcard
                         send_single_dblk,   -- sends a data block
                         wait_sdblock_sent,  -- waits until dblock is sent
                         write_mult_cmd25,   -- start sending write multiple block command
                         wait_wmcmd25_resp,  -- waits for response from SDcard
                         send_mult_dblk,     -- sends a data block in multiple write mode
                         wait_mdblock_sent,  -- waits until dblock is sent in multiple write mode
                         writem_ready,       -- wait for user input in multiple block write mode
                         prep_cmd55,         -- next command is Acmd (only for SDSC cards who have ACMD23 instead of CMD23)
                         wait_pcmd55_resp,   -- waits for response from SDcard to cmd55
                         prep_cmd23,         -- prepares a given number of blocks
                         wait_pcmd23_resp,   -- waits for response from SDcard to cmd23
                         not_sd_card,        -- card detected but not usable
                         no_sd_card);        -- no card in slot
   signal module_state      : t_module_fsm := wait_pw_on;   -- the current state of the module
   --signal module_next_state : t_module_fsm;  -- the next state of the module (what we want to do next)
   
   signal start_cmd    : boolean;                           -- when True, we are asking to send a new command to the SDcard
   signal cmd_to_send  : std_logic_vector( 5 downto 0);     -- the command to send to the SDcard when start_cmd=True
   signal cmd_data     : std_logic_vector(31 downto 0);     -- the eventual data sent as argument of the command
   signal cmd_send_ack : boolean;                           -- the ack feedback of the command sending FSM
   
   signal exp_long_resp  : boolean;                         -- tells the receive state machine that it should expect a long response
   signal exp_R1b_resp   : boolean;                         -- tells the receive state machine that it should expect a response with busy flag
   signal resp_received  : boolean;                         -- True when we received a data response (whatever CRC may be)
   signal resp_wrongCRC  : boolean;                         -- True when we received a data response but with incorrect CRC 
   signal resp_ACMD_ACK  : boolean;                         -- True when the response from SDcard assert the ACMD command
   signal resp_R3_busy   : boolean;                         -- True when the R3 response from SDcard assert the busy bit
   signal resp_R3_CCS    : boolean;                         -- True when CCS in R3 mean SDHC/SDXC
   signal response       : std_logic_vector(31 downto 0);   -- The content of the response from the SDcard
   signal resp_timeout   : boolean;                         -- No response was received after instruction
   signal dblk_recv      : boolean;                         -- a data block has been received
   signal dblk_CRC16_err : boolean;                         -- an error has been detected on data CRC


   -----------------------------------------------------
   -- information about the Card
   -----------------------------------------------------
   type t_SDcard is (unknown_card, v1SD, v2SD, v2SDHC);
   signal card_type  : t_SDcard := unknown_card;      -- which type of SDcard are we dealing with ?
   signal SDcard_RCA : std_logic_vector(15 downto 0); -- the address of the SDcard on the local bus
   signal nb_blocks  : std_logic_vector(31 downto 0); -- the actual number of datablocks available on the SDcard
--   signal highsp_cap : boolean:=False;                -- when True, the SDcard can achieve 50Mb/s /line
   signal cmd6avail  : boolean:=False;                -- when True, the SDcard can accepts CMD6
   signal active4l   : boolean:=False;                -- True when the SDcard is in 4line transfer mode
   signal activeHS   : boolean:=False;                -- True when the card is in high speed mode

   --------------------------------------------------------
   -- The FSM that actually sends commands
   --------------------------------------------------------
   type t_cmd_send_fsm is (s_idle,       -- no command to transfer
                           s_start_bit,  -- sending start bit
                           s_host_bit,   -- sending host bit
                           s_cmd_num,    -- sending command number
                           s_cmd_data,   -- sending data
                           s_crc7,       -- sending CRC7
                           s_stop);      -- sending stop bit
   signal cmd_send_fsm : t_cmd_send_fsm := s_idle;
   
   signal send_data_cnt : integer range 0 to 31 := 0;                     -- counter to identify the bit sent
   signal CRC7_out      : std_logic_vector( 6 downto 0);                  -- used to compute the CRC of the command
   signal data_send_sr  : std_logic_vector(39 downto 0) := x"FFFFFFFFFF"; -- the shift register used to send commands
   


   --------------------------------------------------------
   -- The FSM that receives responses
   --------------------------------------------------------
   type t_cmd_recv_fsm is (r_idle,       -- no command to transfer
                           r_host_bit,   -- a start bit has been received, the next bit will determine whether the host or the card is speaking
                           r_host_cmd,   -- command from host, simply ignoring cmd number, data and CRC7
                           r_exp_resp,   -- a command has been fully read, so we should get a response, in this state, the timeout timer is running
                           r_cmd_num,    -- receiving command number (should match the last command)
                           r_cmd_data,   -- receiving data
                           r_crc7,       -- receiving CRC7
                           r_stop,       -- receiving stop bit
                           r_wnbusy,     -- waiting for busy flag to be released
                           r_over);      -- response is finished
   signal cmd_recv_fsm : t_cmd_recv_fsm := r_idle;
   
   signal recv_data_cnt : integer range 0 to 127:=0;             -- counter to identify the bit received
   signal CRC7_in       : std_logic_vector(6 downto 0);          -- used to check the CRC of the response
   
   --------------------------------------------------------
   -- Timeout (de)counter
   --------------------------------------------------------
   -- this counter is used for pre init delays and command response timeout.
   signal timeout_cnt   : integer range 0 to INSTR_RESP_TIMEOUT := INIT_POWER_ON_TIMEOUT; -- the timeout of the response
   
   --------------------------------------------------------
   -- The actual control of SDcard inderface
   --------------------------------------------------------
   signal clk_pause       : boolean:=False;                                                  -- when True, the clock is paused.
   signal clk_div         : integer range 0 to SCK_HALF_PERIOD_SLOW := SCK_HALF_PERIOD_SLOW; -- the half-period counter
   signal SDsck           : std_logic := '0';                                                -- the internal value of SD_CLK
   signal synced_cmd      : std_logic;                                                       -- the cmd data received 1bit buffer. may be read at any period time.
   signal synced_CD       : std_logic;                                                       -- synchronized SD_CD signal
   signal synced_WP       : std_logic;                                                       -- synchronized SD_WP signal
   signal cmd_recv_strobe : boolean;                                                         -- True when a new cmd bit should be taken into account.
   signal cmd_send_strobe : boolean;                                                         -- True when a new cmd bit should be sent on the line
   signal SD_DAT_HZ       : boolean;                                                         -- True when we have to put SD_DAT in HighZ
   signal SD_DAT_prep     : std_logic_vector(3 downto 0);                                    -- Data to be output as soon as possible
   signal SD_CMD_HZ       : boolean;                                                         -- True when we have to put SD_CMD in HighZ
   signal SD_CMD_prep     : std_logic;                                                       -- CMD value to be output as soon as possible
   
   --------------------------------------------------------
   -- Data packet reception/emission
   --------------------------------------------------------
   -- The state machine that controls data reception/emission
   type t_packet_reception is (p_idle,                            -- nothing specific about data reception
                               pr_expecting_start,                -- we asked for a packet, so waiting for start bit
                               pr_data_receiving,                 -- we are actually receiving data
                               pr_CRC16_receiving,                -- we are actually receiving CRC16
                               pr_stop_bit,                       -- we are receiving the stop bit
                               ps_start,                          -- next bit is start
                               ps_data_sending,                   -- sending data
                               ps_CRC16_sending,                  -- sending CRC
                               ps_stop_bit,                       -- sending stop bit
                               ps_wait_ack,                       -- host should answer ACK bit
                               ps_read_token,                     -- reading response token from the card
                               ps_wait_host_bsy);                 -- host is sending busy bit
   signal fsm_packet_reception : t_packet_reception;
   signal pr_data_counter      : integer range 0 to 4095;         -- the counter used to proceed in states
   signal sync_pdata_read      : std_logic_vector( 3 downto 0);   -- the incoming packet data after synchronization
   signal CRC16_in             : std_logic_vector(63 downto 0);   -- the CRC16 of data used 4 by 4 (line l uses bits (15 downto 0)*4 +l )
   signal sending_dblk         : boolean;                         -- True when a data block start is sent
   signal dblk_sent            : boolean;                         -- True when a data block has been successfully sent
   signal dblk_data_NOK        : boolean;                         -- True when a data block has been sent but SDcard returns wrong signal
   signal dblk_bad_CRC         : boolean;                         -- True when a data block has been sent but SDcard returns CRC error
   signal dblk_write_ERR       : boolean;                         -- True when a data block has been sent but SDcard returns Write error

   -- data storage to the buffer
   signal last_nible           : boolean;                                        -- True when we are receiving the second nible of a byte
   signal byte_cnt             : integer range 0 to WORD_SIZE_BYTES - 1;         -- the number of remaining bytes to build a word
   signal buff_addr            : integer range 0 to 512*(2**BUFFER_NUM_PW2)/WORD_SIZE_BYTES - 1;  -- the address used by the module for buffer management
   signal nible_to_send        : std_logic_vector(3 downto 0);                                    -- asynch copy of the nible to send to simplify code
   signal data_word            : std_logic_vector ( WORD_SIZE_BITS - 1 downto 0);                 -- data for buffer writes
   signal r_data_word          : std_logic_vector ( WORD_SIZE_BITS - 1 downto 0);                 -- data for buffer writes
   signal write_to_buff        : boolean;                                        -- True when data_word should be written to buffer
   signal read_from_buff       : boolean;                                        -- True when r_data_word is read from buffer

   -- endianness management
   -- starting from v2.0.2, all data is actually managed as little endian. the two following signals are asynchronous copies
   -- of the data input/output with data rearranged to be little endian
   signal data_out_litt_endian : STD_LOGIC_VECTOR (8*(2**WORD_SIZE_PW2)-1 downto 0);
   signal data_in_litt_endian  : STD_LOGIC_VECTOR (8*(2**WORD_SIZE_PW2)-1 downto 0);
begin

   -----------------------------------------------------
   -- Reset management
   -----------------------------------------------------
   process(clk)
   begin
      if rising_edge(clk) then
         if reset = '1' then
            longreset <= True;
         elsif module_state = wait_pw_on then
            longreset <= False;
         end if;
      end if;
   end process;


   -----------------------------------------------------
   -- User interface
   -----------------------------------------------------
   -- first we put prebuffering if required
   input_param_prebuffering : if PARAMETER_BUFFERING generate
      process(clk)
      begin
         if rising_edge(clk) then
            if module_state = ready or module_state = erase_m_ready then
               SD_block_local_cpy <= SD_block;
            end if;
         end if;
      end process;
   end generate;
   
   -- if no prebuffering, we just copy input values
   input_param_direct_usage : if not PARAMETER_BUFFERING generate
      SD_block_local_cpy <= SD_block;
   end generate;


   -- these processes actually manages Data transfered to and from the buffers RAM
   -- RAM management was split in two processes since version 2.1.0 to ensure Vivado compliency
   vivado_specific : if VIVADO_SYNTH generate
      -- 1st part, internal buffer access
      process(clk)
      begin
         if rising_edge(clk) then
            if write_to_buff then 
               buffers(buff_addr) <= data_word;
            end if;
            r_data_word <= buffers(buff_addr);
         end if;
      end process;

      -- 2nd part, user buffer access
      user_addr <= loc_buffer & address;   
      process(clk)
      begin
         if rising_edge(clk) then
            if data_write = '1' then
               buffers(to_integer(unsigned(user_addr)))<= data_in_litt_endian;
            end if;
            data_out_litt_endian <= buffers(to_integer(unsigned(user_addr)));
         
         end if;
      end process;
   end generate;

   ise_specific : if not VIVADO_SYNTH generate
      user_addr <= loc_buffer & address;   
      process(clk)
      begin
         if rising_edge(clk) then
            if write_to_buff then 
               buffers(buff_addr) <= data_word;
            end if;
            r_data_word <= buffers(buff_addr);

            if data_write = '1' then
               buffers(to_integer(unsigned(user_addr)))<= data_in_litt_endian;
            end if;
            data_out_litt_endian <= buffers(to_integer(unsigned(user_addr)));
         
         end if;
      end process;
   end generate;


   process(clk)
   begin
      if rising_edge(clk) then
         if reset = '1' then
            err_code <= NO_ERROR;
         --elsif module_state = no_sd_card or module_state = not_sd_card then
         elsif module_state = no_sd_card then 
            err_code <= NO_CARD;
         elsif module_state = not_sd_card then
            err_code <= INVALID_CARD;
         elsif module_state = wait_icmd9_resp and cmd_recv_strobe and cmd_recv_fsm=r_cmd_data and recv_data_cnt = 118 then
            if card_type = v1SD xor synced_cmd = '0'  then
               err_code <= INCONSISTENT_CSD;
            end if;
         elsif (read_block or write_block or erase_block) = '1' and unsigned(SD_block)>unsigned(nb_blocks) then
            err_code <= INVALID_DATA_BLOCK;
         elsif dblk_CRC16_err or dblk_bad_CRC then
            err_code <= TOO_MANY_CRC_RETRIES;
         elsif dblk_write_ERR then
            err_code <= WRITE_ERROR_FROM_CARD;
         elsif cmd_recv_fsm = r_exp_resp and resp_timeout and module_state = wait_pcmd23_resp then
            err_code <= CMD23_TIMEOUT;
         elsif cmd_recv_fsm = r_exp_resp and resp_timeout and module_state /= wait_icmd0_resp
                                                          and module_state /= init_cmd8        then
                                                          --and module_state /= wait_pcmd23_resp then -- this line is not necessary because of the previous test
            err_code <= CARD_RESP_TIMEOUT;
         elsif (module_state = readm_ready or module_state = writem_ready or module_state = erase_m_ready)
            and multiple = '0' then
            err_code <= MULT_OP_CANCELLED;
         elsif synced_WP = '1' and (write_block or erase_block) = '1' then
            err_code <= NO_W_ON_WP_CARD;
         elsif busy_i = '0' then
            err_code <= NO_ERROR;
         end if;
      end if;
   end process;


   -----------------------------------------------------
   -- Main FSM of the module
   -----------------------------------------------------
   process(clk)
   begin
      -- the FSM is not documented but is rather linear... sorry
      -- FIXME : this FSM is incomplete because most commands lack the timeout management...
      if rising_edge(clk) then
         if longreset then
            module_state <= wait_pw_on;
         elsif synced_CD = '1' then
            module_state <= no_sd_card;
         else
            case module_state is
               when wait_pw_on        => if resp_timeout  then    module_state <= wait_72clocks;     end if;
               when wait_72clocks     =>                          module_state <= init_cmd0;
               when init_cmd0         => if cmd_send_ack  then    module_state <= wait_icmd0_resp;   end if;
               when wait_icmd0_resp   => if resp_timeout then     module_state <= init_cmd8;         end if; 
               when init_cmd8         => if cmd_send_ack  then    module_state <= wait_icmd8_resp;   end if;
               when wait_icmd8_resp   => if resp_received then 
                                            if resp_wrongCRC then module_state <= init_cmd8;
                                                             else module_state <= init_cmd55;        end if;
                                      elsif resp_timeout then     module_state <= init_cmd55;        end if;
               when init_cmd55        => if cmd_send_ack  then    module_state <= wait_icmd55_resp;  end if;
               when wait_icmd55_resp  => if resp_received then 
                                            if resp_wrongCRC then module_state <= init_cmd55;
                                         elsif not resp_ACMD_ACK then module_state <= init_cmd55;
                                                             else module_state <= init_acmd41;       end if;
                                      elsif resp_timeout  then    module_state <= not_sd_card;       end if;
               when init_acmd41       => if cmd_send_ack  then    module_state <= wait_iacmd41_resp; end if;
               when wait_iacmd41_resp => if resp_received then -- the response of this command is not CRC7 protected
                                            if resp_R3_busy  then module_state <= init_cmd55;
                                                             else module_state <= init_cmd2;         end if; end if;
               when init_cmd2         => if cmd_send_ack  then    module_state <= wait_icmd2_resp;   end if;
               when wait_icmd2_resp   => if resp_received then 
                                            if resp_wrongCRC then module_state <= init_cmd2;
                                                             else module_state <= init_cmd3;         end if; end if;
               when init_cmd3         => if cmd_send_ack  then    module_state <= wait_icmd3_resp;   end if;
               when wait_icmd3_resp   => if resp_received then 
                                            if resp_wrongCRC then module_state <= init_cmd3;
                                                             else module_state <= init_cmd9;         end if; end if;
               when init_cmd9         => if cmd_send_ack  then    module_state <= wait_icmd9_resp;   end if;
               when wait_icmd9_resp   => if resp_received then 
                                            if resp_wrongCRC then module_state <= init_cmd9;
                                                             else module_state <= init_cmd7;         end if; end if;
               when init_cmd7         => if cmd_send_ack  then    module_state <= wait_icmd7_resp;   end if;
               when wait_icmd7_resp   => if resp_received then 
                                            if resp_wrongCRC then module_state <= init_cmd7;
                                                             else module_state <= init_cmd55b;       end if; end if;
               when init_cmd55b       => if cmd_send_ack  then    module_state <= wait_icmd55b_resp; end if;
               when wait_icmd55b_resp => if resp_received then 
                                            if resp_wrongCRC then module_state <= init_cmd55b;
                                         elsif not resp_ACMD_ACK then module_state <= init_cmd55b;
                                                             else module_state <= init_acmd42;       end if; end if;
               when init_acmd42       => if cmd_send_ack  then    module_state <= wait_iacmd42_resp; end if;         -- pullup disconnection
               when wait_iacmd42_resp => if resp_received then
                                            if resp_wrongCRC then module_state <= init_cmd55b;
                                            elsif cmd6avail and HIGH_SPEED_IF then module_state <= init_cmd6r;
                                                             else module_state <= init_cmd55c;       end if; end if;
               when init_cmd6r        => if cmd_send_ack  then    module_state <= wait_icmd6r_resp;  end if;
               when wait_icmd6r_resp  => if resp_received then
                                            if resp_wrongCRC then module_state <= init_cmd6r;
                                                             else module_state <= wait_icmd6r_status; end if; end if;
               when wait_icmd6r_status=> if dblk_recv     then    module_state <= init_cmd6w;        end if;
               when init_cmd6w        => if cmd_send_ack  then    module_state <= wait_icmd6w_resp;  end if;
               when wait_icmd6w_resp  => if resp_received then
                                            if resp_wrongCRC then module_state <= init_cmd6w;
                                                             else module_state <= wait_icmd6w_status; end if; end if;
               when wait_icmd6w_status=> if dblk_recv     then    module_state <= wait_speed_trans;  end if;
               when wait_speed_trans  => if resp_timeout  then    module_state <= init_cmd55c;       end if;
               when init_cmd55c       => if cmd_send_ack  then    module_state <= wait_icmd55c_resp; end if;      -- prepare 4lines IF transition
               when wait_icmd55c_resp => if resp_received then 
                                              if resp_wrongCRC then module_state <= init_cmd55c;
                                         elsif not resp_ACMD_ACK then module_state <= init_cmd55c;
                                                             else module_state <= init_acmd6;        end if; end if;
               when init_acmd6        => if cmd_send_ack  then    module_state <= wait_iacmd6_resp;  end if;
               when wait_iacmd6_resp  => if resp_received then
                                            if resp_wrongCRC then module_state <= init_cmd55c;
                                                             else module_state <= ready;             end if; end if;
               when ready             => if read_block='1' then 
                                            if multiple='1'  then module_state <= readm_cmd18;
                                                             else module_state <= read_cmd17;        end if;
                                      elsif erase_block='1' and synced_WP = '0' then
                                            if multiple='1'  then module_state <= erase_mult_cmd32;
                                                             else module_state <= erase_cmd32;       end if;
                                      elsif write_block='1' and synced_WP = '0' then
                                            if multiple='1'  then module_state <= write_mult_cmd25;
                                                             else module_state <= write_cmd24;       end if;
                                      elsif prep_block = '1' then 
                                            if card_type = v2SDHC then module_state <= prep_cmd23;   
                                                                  else module_state <= prep_cmd55;   end if; end if;
               when read_cmd17        => if cmd_send_ack  then    module_state <= wait_rcmd17_resp;  end if;
               when wait_rcmd17_resp  => if resp_received then 
                                            if resp_wrongCRC then module_state <= read_cmd17;        -- fixme: this might enter a loop :/
                                                             else module_state <= wait_rcmd17_dblk;  end if; end if;
               when wait_rcmd17_dblk  => if dblk_recv     then
                                             if dblk_CRC16_err then module_state <= read_cmd17;
                                                               else module_state <= ready;           end if; end if;
               when readm_cmd18       => if cmd_send_ack  then    module_state <= wait_rcmd18_resp;  end if;
               when wait_rcmd18_resp  => if resp_received then    module_state <= wait_rcmd18_dblk;  end if;
               when wait_rcmd18_dblk  => if dblk_recv     then
                                             if multiple='0' then module_state <= stop_mread_cmd12;
                                                             else module_state <= readm_ready;       end if; end if;
               when readm_ready       => if multiple= '0' then    module_state <= stop_mread_cmd12;
                                      elsif read_block = '1' then module_state <= wait_rcmd18_dblk;  end if;
               when stop_mread_cmd12  => if cmd_send_ack  then    module_state <= wait_rmread_cmd12; end if;
               when wait_rmread_cmd12 => if resp_received then    module_state <= ready;             end if;
               when erase_mult_cmd32  => if cmd_send_ack  then    module_state <= wait_mecmd32_resp; end if;
               when wait_mecmd32_resp => if resp_received then    module_state <= erase_m_ready;     end if;
               when erase_m_ready     => if multiple='0'  then    module_state <= ready;
                                      elsif erase_block='1' then  module_state <= erase_cmd33;       end if;
               when erase_cmd32       => if cmd_send_ack  then    module_state <= wait_ecmd32_resp;  end if;
               when wait_ecmd32_resp  => if resp_received then    module_state <= erase_cmd33;       end if;
               when erase_cmd33       => if cmd_send_ack  then    module_state <= wait_ecmd33_resp;  end if;
               when wait_ecmd33_resp  => if resp_received then    module_state <= erase_cmd38;       end if;
               when erase_cmd38       => if cmd_send_ack  then    module_state <= wait_ecmd38_resp;  end if;
               when wait_ecmd38_resp  => if resp_received then    module_state <= ready;             end if;
               when write_cmd24       => if cmd_send_ack  then    module_state <= wait_wcmd24_resp;  end if;
               when wait_wcmd24_resp  => if resp_received then    module_state <= send_single_dblk;  end if;
               when send_single_dblk  => if sending_dblk  then    module_state <= wait_sdblock_sent; end if;
               when wait_sdblock_sent => if dblk_sent     then 
                                            if dblk_bad_CRC then  module_state <= write_cmd24;       
                                                            else  module_state <= ready;             end if; end if;
               when write_mult_cmd25  => if cmd_send_ack  then    module_state <= wait_wmcmd25_resp; end if;
               when wait_wmcmd25_resp => if resp_received then    module_state <= send_mult_dblk;    end if;
               when send_mult_dblk    => if sending_dblk  then    module_state <= wait_mdblock_sent; end if;
               when wait_mdblock_sent => if dblk_sent     then
                                             if multiple='0' then module_state <= stop_mread_cmd12;
                                                             else module_state <= writem_ready;      end if; end if;
               when writem_ready      => if multiple='0'  then    module_state <= stop_mread_cmd12;
                                      elsif write_block='1' then  module_state <= send_mult_dblk;    end if;
               when prep_cmd55        => if cmd_send_ack  then    module_state <= wait_pcmd55_resp;  end if;
               when wait_pcmd55_resp  => if resp_received then 
                                            if resp_wrongCRC then module_state <= prep_cmd55;
                                         elsif not resp_ACMD_ACK then module_state <= prep_cmd55;
                                                             else module_state <= prep_cmd23;        end if; end if;
               when prep_cmd23        => if cmd_send_ack  then    module_state <= wait_pcmd23_resp;  end if;
               when wait_pcmd23_resp  => if resp_received or resp_timeout then    module_state <= ready; end if;
               when not_sd_card       => null;
               when no_sd_card           => if synced_CD='0' then    module_state <= wait_pw_on;        end if;
            end case;
         end if;
      end if;
   end process;

   exp_long_resp <= module_state = wait_icmd9_resp  or module_state = wait_icmd2_resp;
   exp_R1b_resp  <= module_state = wait_icmd7_resp or module_state = wait_ecmd38_resp or module_state = wait_rmread_cmd12;
   dblk_recv     <= fsm_packet_reception = pr_stop_bit and cmd_recv_strobe;
   
   process(module_state, card_type, SD_block_local_cpy, SDcard_RCA)
   begin
      case module_state is
         when init_cmd0        => start_cmd   <= True;
                                  cmd_to_send <= "000000";
                                  cmd_data    <= X"00000000";           -- 32 stuff bits
         when init_cmd8        => start_cmd   <= True;
                                  cmd_to_send <= "001000";
                                  cmd_data    <= X"000001AA";           
         when init_cmd55       => start_cmd   <= True;
                                  cmd_to_send <= "110111";
                                  cmd_data    <= X"0000" & x"0000";    -- RCA16(zeroed) & 16 stuff bits
         when init_acmd41      => start_cmd   <= True;
                                  cmd_to_send <= "101001";
                                  if card_type = v1SD then
                                    cmd_data    <= X"00FF8000";
                                  else
                                    cmd_data    <= X"50FF8000";
                                  end if;
         when init_cmd2        => start_cmd   <= True;
                                  cmd_to_send <= "000010";
                                  cmd_data    <= X"00000000";          -- 32 stuff bits
         when init_cmd3        => start_cmd   <= True;
                                  cmd_to_send <= "000011";
                                  cmd_data    <= X"00000000";          -- 32 stuff bits
         when init_cmd9        => start_cmd   <= True;
                                  cmd_to_send <= "001001";
                                  cmd_data    <= SDcard_RCA & X"0000"; -- 16 stuff bits
         when init_cmd7        => start_cmd   <= True;
                                  cmd_to_send <= "000111";
                                  cmd_data    <= SDcard_RCA & X"0000"; -- 16 stuff bits
         when init_cmd55b
            | init_cmd55c
            | prep_cmd55       => start_cmd   <= True;
                                  cmd_to_send <= "110111";
                                  cmd_data    <= SDcard_RCA & X"0000"; -- 16 stuff bits
         when init_acmd42      => start_cmd   <= True;
                                  cmd_to_send <= "101010";
                                  cmd_data    <= x"0000000" & "0000";  -- first 31 bits are stuff bits
         when init_acmd6       => start_cmd   <= True;
                                  cmd_to_send <= "000110";
                                  cmd_data    <= x"0000000" & "0010";  -- first 30 bits are stuff bits
         when init_cmd6r       => start_cmd   <= True;
                                  cmd_to_send <= "000110";
                                  cmd_data    <= x"0000FFFF"; -- get information on switch functions
         when init_cmd6w       => start_cmd   <= True;
                                  cmd_to_send <= "000110";
                                  cmd_data    <= x"8000FFF1"; -- switch to high speed
         when read_cmd17       => start_cmd   <= True;
                                  cmd_to_send <= "010001";
                                  if card_type = v1SD then
                                    cmd_data  <= SD_block_local_cpy(22 downto 0) & "000000000";
                                  else
                                    cmd_data  <= SD_block_local_cpy;
                                  end if;
         when readm_cmd18      => start_cmd   <= True;
                                  cmd_to_send <= "010010";
                                  if card_type = v1SD then
                                    cmd_data  <= SD_block_local_cpy(22 downto 0) & "000000000";
                                  else
                                    cmd_data  <= SD_block_local_cpy;
                                  end if;
         when stop_mread_cmd12 => start_cmd   <= True;
                                  cmd_to_send <= "001100";
                                  cmd_data    <= X"00000000";          -- 32 stuff bits
         when erase_cmd32 
            | erase_mult_cmd32 => start_cmd   <= True;
                                  cmd_to_send <= "100000";
                                  if card_type = v1SD then
                                    cmd_data  <= SD_block_local_cpy(22 downto 0) & "000000000";
                                  else
                                    cmd_data  <= SD_block_local_cpy;
                                  end if;
         when erase_cmd33      => start_cmd   <= True;
                                  cmd_to_send <= "100001";
                                  if card_type = v1SD then
                                    cmd_data  <= SD_block_local_cpy(22 downto 0) & "000000000";
                                  else
                                    cmd_data  <= SD_block_local_cpy;
                                  end if;
         when erase_cmd38      => start_cmd   <= True;
                                  cmd_to_send <= "100110";
                                  cmd_data    <= X"00000000";          -- 32 stuff bits
         when write_cmd24      => start_cmd   <= True;
                                  cmd_to_send <= "011000";
                                  if card_type = v1SD then
                                    cmd_data  <= SD_block_local_cpy(22 downto 0) & "000000000";
                                  else
                                    cmd_data  <= SD_block_local_cpy;
                                  end if;
         when write_mult_cmd25 => start_cmd   <= True;
                                  cmd_to_send <= "011001";
                                  if card_type = v1SD then
                                    cmd_data  <= SD_block_local_cpy(22 downto 0) & "000000000";
                                  else
                                    cmd_data  <= SD_block_local_cpy;
                                  end if;
         when prep_cmd23       => -- this command is a base command of SDHC/SDXC cards, and ACMD command in SDSC
                                  -- SDSC cards require 9 upper bits of cmd_data to be stuff bits, this is not a problem
                                  -- since their number of block cannot exceed 2^22
                                  start_cmd   <= True;
                                  cmd_to_send <= "010111";
                                  cmd_data    <= SD_block_local_cpy; 
         when others           => start_cmd   <= False;
                                  cmd_to_send <= "111111";
                                  cmd_data    <= X"00000000";
      end case;
   end process;

   busy   <= busy_i;
   busy_i <= '0' when module_state = ready        or module_state = readm_ready 
                   or module_state = writem_ready or module_state = erase_m_ready   else '1';



   -----------------------------------------------------
   -- information about the Card
   -----------------------------------------------------
   -- What kind of SDcard are we talking to ?
   process(clk)
   begin
      if rising_edge(clk) then
         if (longreset and module_state = wait_pw_on) or synced_CD = '1' then
            card_type <= unknown_card;
         elsif module_state = init_cmd8 then
            card_type <= v1SD;
         elsif module_state = wait_icmd8_resp and resp_received and not resp_wrongCRC then
            card_type <= v2SD;
         elsif module_state = wait_iacmd41_resp and resp_received and (not resp_R3_busy) and (resp_R3_CCS)then
            card_type <= v2SDHC;
         end if;
      end if;
   end process;

   -- the card address on the local bus (even if it is the only one :/ )
   process(clk)
   begin
      if rising_edge(clk) then
         if module_state = wait_icmd3_resp and resp_received then
            SDcard_RCA <= response(31 downto 16);
         end if;
      end if;
   end process;




   -- Let's compute the number of data blocks...
   blocks <= nb_blocks;
   process(clk)
      variable shift : integer range 0 to 15;
   begin
      -- in this process, bit numbers must be taken 1 bit later because the considered bit
      -- hasn't be stored in 'response' yet
      -- data is read with a shift of 8bits because datasheet takes CRC in the bit counting
      if rising_edge(clk) then
         if (longreset and module_state = wait_pw_on) or synced_CD = '1' then
            nb_blocks <= x"00000000";
         elsif card_type = v1SD then
            -- here we are reading the CSDv1.0 format
            if  module_state = wait_icmd9_resp and cmd_recv_strobe and cmd_recv_fsm=r_cmd_data then
               if recv_data_cnt = 71 then
                  -- first we read READ_BL_LEN
                  nb_blocks(31 downto 28) <= response( 3 downto 0);
               elsif recv_data_cnt = 53 then
                  -- then C_SIZE
                  nb_blocks(11 downto  0) <= std_logic_vector(unsigned(response(11 downto 0)) + 1);
               elsif recv_data_cnt = 38 then
                  -- then C_SIZE_MULT wich is directly used through 'shift'
                  
                  -- the following part of code is fully functionnal with ISE, but is not
                  -- supported by Vivado (complex assignements). It has been replaced by
                  -- equivalent code supported by both tools, but remains in place because
                  -- of its reading clarity
                  ----------------------------------------------------------------------------
                  --      shift := to_integer(unsigned(response(2 downto 0)));
                  --      case nb_blocks(31 downto 28) is
                  --         when "1001" =>
                  --            nb_blocks(shift +  1 downto 0)          <= (others => '0');
                  --            nb_blocks(shift + 13 downto shift +  2) <= nb_blocks(11 downto 0);
                  --            nb_blocks(        31 downto shift + 14) <= (others => '0');
                  --         when "1010" =>
                  --            nb_blocks(shift +  2 downto 0)          <= (others => '0');
                  --            nb_blocks(shift + 14 downto shift +  3) <= nb_blocks(11 downto 0);
                  --            nb_blocks(        31 downto shift + 15) <= (others => '0');
                  --         when others =>
                  --            nb_blocks(shift +  3 downto 0)          <= (others => '0');
                  --            nb_blocks(shift + 15 downto shift +  4) <= nb_blocks(11 downto 0);
                  --            nb_blocks(        31 downto shift + 16) <= (others => '0');
                  --      end case;
                          
                  shift := to_integer(unsigned(response(2 downto 0)));
                  case nb_blocks(31 downto 28) is
                    when "1001" => shift := shift + 2;
                    when "1010" => shift := shift + 3;
                    when others => shift := shift + 4;
                  end case;
                  for i in 0 to 31 loop
                    if i<shift or i > shift + 13 then
                        nb_blocks(i) <= '0';
                    else
                        nb_blocks(i) <= nb_blocks(i-shift);
                    end if; 
                  end loop;
                  ------------ End of replacement code -------------------------
                  
               end if;
            end if;
         else
            -- here we are reading the CSDv2.0 format
            if  module_state = wait_icmd9_resp and cmd_recv_strobe and cmd_recv_fsm=r_cmd_data and recv_data_cnt = 39 then
               nb_blocks <= response(21 downto 0) & "0000000000";
            end if;
         end if;
      end if;
   end process;

--   -- determine the SDcard speed
--   -- This section was removed because it does not show the max speed of the card, but the maw speed of the configuration
--   process(clk)
--   begin
--      -- in this process, bit numbers must be taken 1 bit later because the considered bit
--      -- hasn't be stored in 'response' yet
--      -- data is read with a shift of 8bits because datasheet takes CRC in the bit counting
--      if rising_edge(clk) then
--         if longreset and module_state = wait_pw_on then
--            highsp_cap <= False;
--         elsif  module_state = wait_icmd9_resp and cmd_recv_strobe and cmd_recv_fsm=r_cmd_data and recv_data_cnt = 87 then
--            highsp_cap <= response(7 downto 0) /= x"32";
--         end if;
--      end if;
--   end process;


   -- is cmd6 available ?
   CMD6_required : if HIGH_SPEED_IF generate
      process(clk)
      begin
         -- in this process, bit numbers must be taken 1 bit later because the considered bit
         -- hasn't be stored in 'response' yet
         -- data is read with a shift of 8bits because datasheet takes CRC in the bit counting
         if rising_edge(clk) then
            if longreset and module_state = wait_pw_on then
               cmd6avail <= False;
            elsif module_state = wait_icmd9_resp and cmd_recv_strobe and cmd_recv_fsm=r_cmd_data and recv_data_cnt = 75 then
               cmd6avail <= response(10) = '1'  ;
            end if;
         end if;
      end process;
   end generate;
   CMD6_not_required : if not HIGH_SPEED_IF generate
      cmd6avail <= False;
   end generate;
   
   

   -- in which line mode are we transferring ?
   process(clk)
   begin
      if rising_edge(clk) then
         if longreset and module_state = wait_pw_on then
            active4l <= False;
         elsif  module_state = init_acmd6 then
            active4l <= True;
         end if;
      end if;
   end process;

   -- in which speed mode are we transferring ?
   process(clk)
   begin
      if rising_edge(clk) then
         if longreset and module_state = wait_pw_on then
            activeHS <= False;
         elsif  module_state = wait_speed_trans and resp_timeout then
            activeHS <= True;
         end if;
      end if;
   end process;




   --------------------------------------------------------
   -- The FSM that actually sends commands
   --------------------------------------------------------
   -- The state of the FSM is what we want to output next time
   process(clk)
   begin
      if rising_edge(clk) then
         if longreset and module_state = wait_pw_on then
            cmd_send_fsm <= s_idle;
         elsif cmd_send_strobe then
            case cmd_send_fsm is
               when s_idle      => if start_cmd         then cmd_send_fsm <= s_start_bit; end if;
               when s_start_bit =>                           cmd_send_fsm <= s_host_bit;
               when s_host_bit  =>                           cmd_send_fsm <= s_cmd_num;
               when s_cmd_num   => if send_data_cnt = 0 then cmd_send_fsm <= s_cmd_data;  end if;
               when s_cmd_data  => if send_data_cnt = 0 then cmd_send_fsm <= s_crc7;      end if;
               when s_crc7      => if send_data_cnt = 0 then cmd_send_fsm <= s_stop;      end if;
               when s_stop      =>                           cmd_send_fsm <= s_idle;
            end case;
         end if;
      end if;
   end process;

   -- here, we manage the data_counter
   process(clk)
   begin
      if rising_edge(clk) then
         if  longreset and module_state = wait_pw_on then
            send_data_cnt <= 0;
         elsif send_data_cnt > 0 and cmd_send_strobe then
            send_data_cnt <= send_data_cnt - 1;
         elsif cmd_send_strobe then
            case cmd_send_fsm is
               when s_idle      => send_data_cnt <= 0;
               when s_start_bit => send_data_cnt <= 0;
               when s_host_bit  => send_data_cnt <= 5;   -- 6 cmd bits
               when s_cmd_num   => send_data_cnt <= 31;  -- 32 data bits
               when s_cmd_data  => send_data_cnt <= 7;   -- 8 CRC bits
               when s_crc7      => send_data_cnt <= 0;
               when s_stop      => send_data_cnt <= 0;
            end case;
         end if;
      end if;
   end process;

   process(clk)
   begin
      if rising_edge(clk) then
         if longreset then
            data_send_sr <= (others => '1');
         elsif cmd_send_strobe then
            if start_cmd and cmd_send_fsm = s_idle then
               data_send_sr <= "01" & cmd_to_send & cmd_data;  
            elsif cmd_send_fsm = s_cmd_data and send_data_cnt = 0 then
               data_send_sr(39 downto 33) <= CRC7_out;
            else
               data_send_sr <= data_send_sr(38 downto 0) & '1';
            end if;
         end if;
      end if;
   end process;

   cmd_send_ack <= cmd_send_fsm = s_stop and cmd_send_strobe;
   
   -- computing the CRC of the sent command...
   process(clk)
   begin
      if rising_edge(clk) then
         if cmd_send_strobe then
            if cmd_send_fsm = s_idle then 
               CRC7_out <= "0000000";
            else
               -- crc is computed on bit 38 of the shift register
               -- this excludes computation on the first bit, this is not a problem since this bit is always 0.
               -- CRC is computed one bit 'earlier', then we avoid the problem of updating CRC7 and using it at
               -- the same time to feed the shift register.
               CRC7_out <= CRC7_out(5 downto 3) & (CRC7_out(6) xor CRC7_out(2) xor data_send_sr(38)) & CRC7_out(1 downto 0) & (CRC7_out(6) xor data_send_sr(38));
            end if;
         end if;
      end if;
   end process;

   --------------------------------------------------------
   -- The FSM that receives responses
   --------------------------------------------------------
   -- the FSM state actually codes what we just have received
   process(clk)
   begin
      if rising_edge(clk) then
         if longreset and module_state = wait_pw_on then
            cmd_recv_fsm <= r_idle;
         elsif cmd_send_fsm = s_stop and cmd_send_strobe then
            -- host just sent the stop bit, so we prepare to receive a response
            cmd_recv_fsm <= r_exp_resp;
         elsif cmd_send_fsm /= s_idle then
            -- host is sending a command, just ignore it
            cmd_recv_fsm <= r_host_cmd;
         elsif cmd_recv_strobe then
            case cmd_recv_fsm is
               when r_idle     => if synced_cmd = '0'  then cmd_recv_fsm <= r_host_bit; end if; -- should only happen after a timeout
               when r_host_bit =>                           cmd_recv_fsm <= r_cmd_num;          -- we received the second bit, it determines if it is a response, if not an error will be produced elsewhere
               when r_host_cmd =>                           cmd_recv_fsm <= r_exp_resp;         -- should never happen, just to be sure...
               when r_exp_resp => if synced_cmd = '0'  then cmd_recv_fsm <= r_host_bit;
                               elsif timeout_cnt = 0   then cmd_recv_fsm <= r_idle;     end if; -- we received a start bit
               when r_cmd_num  => if recv_data_cnt = 0 then cmd_recv_fsm <= r_cmd_data; end if; -- we received the command num, we get the data
               when r_cmd_data => if recv_data_cnt = 0 then cmd_recv_fsm <= r_crc7;     end if; -- we received the data, we get the crc
               when r_crc7     => if recv_data_cnt = 0 then cmd_recv_fsm <= r_stop;     end if; -- we received the CRC, we get the stop bit
               when r_stop     => if recv_data_cnt = 0 then
                                     if exp_R1b_resp      then cmd_recv_fsm <= r_wnbusy;           -- if busy flag is to expect, we go to specific state
                                                          else cmd_recv_fsm <= r_over;     end if; end if; -- command is over,
               when r_wnbusy   => if sync_pdata_read(0) = '1' then cmd_recv_fsm <= r_over; end if; -- busy has been released
               when r_over     =>                           cmd_recv_fsm <= r_idle;             -- sending ack to module_fsm, return to idle state
            end case;
--         else
--            -- the following states do not rely on bit sending... so we manage them even if cmd_recv_strobe is false
--            case cmd_recv_fsm is
--               when r_wnbusy   => if sync_pdata_read(0) = '1' then cmd_recv_fsm <= r_over; end if; -- busy has been released
--               when r_over     =>                           cmd_recv_fsm <= r_idle;             -- sending ack to module_fsm, return to idle state
--               when others     => null;
--            end case;
            
         end if;
      end if;
   end process;


   -- the data counter
   process(clk)
   begin
      if rising_edge(clk) then
         if longreset and module_state = wait_pw_on then
            recv_data_cnt <= 0;
         elsif recv_data_cnt > 0 and cmd_recv_strobe then
            recv_data_cnt <= recv_data_cnt - 1;
         elsif cmd_recv_strobe then
            case cmd_recv_fsm is
               when r_idle     => recv_data_cnt <= 0;
               when r_host_bit => recv_data_cnt <= 5;                                  -- we prepare the value for the r_cmd_num state
               when r_host_cmd => recv_data_cnt <= 0;
               when r_exp_resp => recv_data_cnt <= 0;
               when r_cmd_num  => if exp_long_resp  then recv_data_cnt <= 119;         -- we expect a long response,
                                                    else recv_data_cnt <= 31;  end if; -- we expect a normal response
               when r_cmd_data => recv_data_cnt <= 6;                                  -- we prepare the length for CRC7
               when r_crc7     => recv_data_cnt <= 7;                                  -- each command should have 8 stop bits
               when r_stop     => recv_data_cnt <= 0;
               when r_wnbusy   => recv_data_cnt <= 0;
               when r_over     => recv_data_cnt <= 0;
            end case;
         end if;
      end if;
   end process;


   -- storing the response value
   process(clk)
   begin
      if rising_edge(clk) then
         if cmd_recv_fsm = r_cmd_data and cmd_recv_strobe then
            response <= response(30 downto 0) & synced_cmd;
         elsif cmd_recv_fsm = r_idle and fsm_packet_reception = ps_read_token and cmd_recv_strobe then
            response(3 downto 0) <= response(2 downto 0) & sync_pdata_read(0);
         elsif cmd_recv_fsm = r_idle and fsm_packet_reception = pr_data_receiving and cmd_recv_strobe then
            response(15 downto 0) <= response(14 downto 0) & sync_pdata_read(0);
         end if;
      end if;
   end process;

   
   -- now we compute the CRC7 of the response.
   process(clk)
   begin
      if rising_edge(clk) then
         if cmd_recv_fsm = r_exp_resp then 
            CRC7_in <= "0000000";            -- we clear the CRC when we are actually waiting for a response
                                             -- first bit will not be included, not a problem since it is a first 0
                                             -- and it does not change the CRC
         elsif exp_long_resp and cmd_recv_fsm = r_cmd_num then
            CRC7_in <= "0000000";            -- we compute the CRC of the data only for long response.
         elsif cmd_recv_strobe and (cmd_recv_fsm = r_crc7) then
                                             -- while we receive the CRC, we shift the computed one, to see if bits match
            CRC7_in <= CRC7_in(5 downto 0) & '0';
         elsif cmd_recv_strobe  then
                                             -- the actual computation of the CRC
            CRC7_in <= CRC7_in(5 downto 3) & (CRC7_in(6) xor CRC7_in(2) xor synced_cmd) & CRC7_in(1 downto 0) & (CRC7_in(6) xor synced_cmd);
         end if;
      end if;
   end process;

   
   -- informing the module FSM of the result...
   resp_received <= cmd_recv_fsm = r_over and cmd_recv_strobe;
   resp_ACMD_ACK <= response( 5) = '1';
   resp_R3_busy  <= response(31) = '0';
   resp_R3_CCS   <= response(30) = '1'; -- True when SDHC/SDXC
   
   process(clk)
   begin
      if rising_edge(clk) then
         if cmd_recv_fsm = r_idle then
            resp_wrongCRC <= False;
--         elsif cmd_recv_strobe and (cmd_recv_fsm = r_crc7) then
--            -- we set the WrongCRCbit if mismatch during CRC cheg phase
--            resp_wrongCRC <= resp_wrongCRC or (synced_cmd /= CRC7_in(6));
         else
            -- otherwise, we clear it on response receive.
            resp_wrongCRC <= resp_wrongCRC and not resp_received;
         end if;
      end if;
   end process;


   --------------------------------------------------------
   -- Timeout (de)counter
   --------------------------------------------------------
   -- this design is a bit buggy because if card is inserted after reset, the timeout
   -- will be INSTR_RESP_TIMEOUT instead of INIT_POWER_ON_TIMEOUT. This is not a problem
   -- because inserting the SDcard is not millisecond precise in timing :)
   process(clk)
   begin
      if rising_edge(clk) then
         if longreset then
            timeout_cnt  <= INIT_POWER_ON_TIMEOUT;
         elsif module_state = wait_pw_on then
            if timeout_cnt > 0 then
               timeout_cnt  <= timeout_cnt - 1;
            end if;
         elsif module_state = init_cmd0 then
            timeout_cnt  <= INIT_CMD0_TIMEOUT;
         elsif module_state = wait_icmd0_resp then
            if timeout_cnt > 0 then
               timeout_cnt  <= timeout_cnt - 1;
            end if;
         elsif module_state = wait_icmd6w_status then
            timeout_cnt  <= INIT_FREQ_SW_TIMEOUT;
         elsif module_state = wait_speed_trans then
            if timeout_cnt > 0 then
               timeout_cnt  <= timeout_cnt - 1;
            end if;            
         elsif cmd_recv_fsm /= r_exp_resp then
            timeout_cnt  <= INSTR_RESP_TIMEOUT;
         elsif timeout_cnt > 0 then
            timeout_cnt <= timeout_cnt - 1;
         else
            timeout_cnt  <= INSTR_RESP_TIMEOUT;
         end if;
      end if;
   end process;
   
   resp_timeout <= timeout_cnt = 0;
   

   --------------------------------------------------------
   -- Managing the low level signaling of the SDcard bus...
   --------------------------------------------------------

   -- Sometimes, we should Stop the clock...
   -- FIXME : usefull to get this signal synchronous ?
   process(clk)
   begin
      if rising_edge(clk) then
         if  longreset then
            clk_pause <= False;  -- by default, we should not force clock to pause during reset
                                 -- this is to give the opportunity to leave specific states
                                 -- of the SDcard at the beginning of a reset
         elsif module_state = readm_ready  then
            clk_pause <= True;   -- Clock is paused when waiting for next block read.
         else
            clk_pause <= False;  -- Normal operation, the clock should work.
         end if;
      end if;
   end process;

   -- The clock itself, with its divider
   process(clk)
   begin
      if rising_edge(clk) then
         if longreset and module_state = wait_pw_on then
            SDsck   <= '0';
            clk_div <= SCK_HALF_PERIOD_SLOW;
         elsif clk_pause then
            null;
         elsif clk_div = 0 then
            SDsck   <= not SDsck;
            if activeHS then
               clk_div <= SCK_HALF_PERIOD_FAST;
            else
               clk_div <= SCK_HALF_PERIOD_SLOW;
            end if;
         else
            clk_div <= clk_div - 1;
         end if;
      end if;
   end process;
   
   SD_CLK <= SDsck;

   cmd_recv_strobe <= (SDsck = '0') and (clk_div = 0) when activeHS
                 else (SDsck = '1') and (clk_div = 0);
   cmd_send_strobe <= (SDsck = '0') and (clk_div = 0) when activeHS
                 else (SDsck = '1') and (clk_div = SCK_HALF_PERIOD_SLOW);
   
   process(clk)
   begin
      if rising_edge(clk) then
         synced_cmd <= SD_CMD;
         synced_CD  <= SD_CD;
         synced_WP  <= SD_WP;
      end if;
   end process;
   
   
   process(clk)
   begin
      if rising_edge(clk) then
         if cmd_send_strobe and cmd_send_fsm = s_idle then
            if cmd_recv_fsm = r_idle  then
               SD_CMD_prep <= '1';
               SD_CMD_HZ   <= False;
            else
               --SD_CMD_prep <= 'Z';
               SD_CMD_HZ   <= True;
            end if;            
         elsif cmd_send_strobe then
            SD_CMD_prep <= data_send_sr(39);
            SD_CMD_HZ   <= False;
         end if;
      end if;
   end process;


   cmd_buffering_slow : if CLK_FREQ_HZ < 200000000 generate
      -- the command is exceptionnally sent on the falling edge of clk. This is definitely not
      -- good practice, but allows 50MHz transfers with a base clock of 100MHz.
      process(clk)
      begin
         if falling_edge(clk) then
            if SD_CMD_HZ then
               SD_CMD <= 'Z';
            else
               SD_CMD <= SD_CMD_prep;
            end if;
         end if;
      end process;
   end generate;

   cmd_buffering_fast : if CLK_FREQ_HZ > 199999999 generate
      -- clean
      process(clk)
      begin
         if rising_edge(clk) then
            if SD_CMD_HZ then
               SD_CMD <= 'Z';
            else
               SD_CMD <= SD_CMD_prep;
            end if;
         end if;
      end process;
   end generate;


   
   --------------------------------------------------------
   -- Data packet reception/emission
   --------------------------------------------------------
   -- The state machine that controls data reception/emission
   process(clk)
   begin
      if rising_edge(clk) then
         if module_state = wait_pw_on or module_state = stop_mread_cmd12 then
                           -- if stopping transfer, we force reception as idle to keep buffer data intact
            fsm_packet_reception <= p_idle;
         else
            case fsm_packet_reception is
               when p_idle             => if cmd_recv_strobe and(module_state = wait_rcmd17_dblk or
                                                                 module_state = wait_rcmd18_dblk or
                                                                 module_state = wait_icmd6r_resp or
                                                                 module_state = wait_icmd6w_resp)then fsm_packet_reception <= pr_expecting_start;
                                       elsif cmd_send_strobe and(module_state = send_single_dblk or 
                                                                 module_state = send_mult_dblk)  then fsm_packet_reception <= ps_start;           end if;
               when pr_expecting_start => if cmd_recv_strobe and sync_pdata_read(0) = '0'
                                          and (sync_pdata_read = "0000" or not active4l)         then fsm_packet_reception <= pr_data_receiving;  end if;
               when pr_data_receiving  => if cmd_recv_strobe and pr_data_counter = 0             then fsm_packet_reception <= pr_CRC16_receiving; end if;
               when pr_CRC16_receiving => if cmd_recv_strobe and pr_data_counter = 0             then fsm_packet_reception <= pr_stop_bit;        end if;
               when pr_stop_bit        => if cmd_recv_strobe and module_state = wait_rcmd18_dblk then fsm_packet_reception <= pr_expecting_start;
                                       elsif cmd_recv_strobe                                     then fsm_packet_reception <= p_idle;             end if;
               when ps_start           => if cmd_send_strobe                                     then fsm_packet_reception <= ps_data_sending;    end if;
               when ps_data_sending    => if cmd_send_strobe and pr_data_counter = 0             then fsm_packet_reception <= ps_CRC16_sending;   end if;
               when ps_CRC16_sending   => if cmd_send_strobe and pr_data_counter = 0             then fsm_packet_reception <= ps_stop_bit;        end if;
               when ps_stop_bit        => if cmd_send_strobe and pr_data_counter = 0             then fsm_packet_reception <= ps_wait_ack;        end if;
               when ps_wait_ack        => if cmd_recv_strobe and sync_pdata_read(0) = '0'        then fsm_packet_reception <= ps_read_token;      end if;
               when ps_read_token      => if cmd_recv_strobe and pr_data_counter = 0             then fsm_packet_reception <= ps_wait_host_bsy;   end if;
               when ps_wait_host_bsy   => if cmd_recv_strobe and sync_pdata_read(0) = '1'        then fsm_packet_reception <= p_idle;             end if;
            end case;
         end if;
      end if;
   end process;
   
   -- inform module_fsm about our status
   sending_dblk    <= fsm_packet_reception = ps_start;
   -- cmd_recv_strobe is mandatory to be sure that the line had time to go up again
   dblk_sent       <= fsm_packet_reception = ps_wait_host_bsy and sync_pdata_read(0) = '1' and cmd_recv_strobe;


   dblk_data_NOK   <= fsm_packet_reception = ps_wait_host_bsy and response(3 downto 0) /="0101";
   dblk_bad_CRC    <= fsm_packet_reception = ps_wait_host_bsy and response(3 downto 0) = "1011";
   dblk_write_ERR  <= fsm_packet_reception = ps_wait_host_bsy and response(3 downto 0) = "1101";
   
   
   
   -- the data counter
   process(clk)
   begin
      if rising_edge(clk) then
         if  module_state = stop_mread_cmd12 then
            pr_data_counter <= 0;
         else
            case fsm_packet_reception is
               when p_idle             =>                                                     pr_data_counter <= 0;
               when pr_expecting_start => if module_state = wait_icmd6r_status or module_state = wait_icmd6w_status then pr_data_counter <= 511;
                                          elsif active4l                                 then pr_data_counter <= 1023;
                                                                                         else pr_data_counter <= 4095;  end if;
               when pr_data_receiving  => if cmd_recv_strobe then if pr_data_counter > 0 then pr_data_counter <= pr_data_counter - 1;
                                                                                         else pr_data_counter <= 15;    end if; end if;
               when pr_CRC16_receiving => if cmd_recv_strobe then if pr_data_counter > 0 then pr_data_counter <= pr_data_counter - 1;
                                                                                         else pr_data_counter <= 0;     end if; end if;
               when pr_stop_bit        =>                                                     pr_data_counter <= 0;
               when ps_start           => if active4l                                    then pr_data_counter <= 1023;
                                                                                         else pr_data_counter <= 4095;  end if;
               when ps_data_sending    => if cmd_send_strobe then if pr_data_counter > 0 then pr_data_counter <= pr_data_counter - 1;
                                                                                         else pr_data_counter <= 15;    end if; end if;
               when ps_CRC16_sending   => if cmd_send_strobe then if pr_data_counter > 0 then pr_data_counter <= pr_data_counter - 1;
                                                                                         else pr_data_counter <= 1;     end if; end if;
               when ps_stop_bit        => if cmd_send_strobe then if pr_data_counter > 0 then pr_data_counter <= pr_data_counter - 1;
                                                                                         else pr_data_counter <= 0;     end if; end if;
               when ps_wait_ack        =>                                                     pr_data_counter <= 3;
               when ps_read_token      => if cmd_recv_strobe then if pr_data_counter > 0 then pr_data_counter <= pr_data_counter - 1;
                                                                                         else pr_data_counter <= 0;     end if; end if;
               when ps_wait_host_bsy   =>                                                     pr_data_counter <= 0;
            end case;
         end if;
      end if;
   end process;
   
   -- this process is to synchronize incoming data signals
   process(clk)
   begin
      if rising_edge(clk) then
         sync_pdata_read <= SD_DAT;
      end if;
   end process;


   process(clk)
   begin
      if rising_edge(clk) then
         case fsm_packet_reception is
            when pr_expecting_start | ps_start | pr_stop_bit | ps_stop_bit =>
               -- just resetting the CRC16_in signal
               CRC16_in <= (others => '0');
            when pr_data_receiving =>
               -- the actual CRC16 computation
               if cmd_recv_strobe then
                  CRC16_in(63 downto 52) <= CRC16_in(59 downto 48);
                  CRC16_in(51 downto 48) <= CRC16_in(47 downto 44) xor CRC16_in(63 downto 60) xor sync_pdata_read;
                  CRC16_in(47 downto 24) <= CRC16_in(43 downto 20);
                  CRC16_in(23 downto 20) <= CRC16_in(19 downto 16) xor CRC16_in(63 downto 60) xor sync_pdata_read;
                  CRC16_in(19 downto  4) <= CRC16_in(15 downto 0);
                  CRC16_in( 3 downto  0) <=                            CRC16_in(63 downto 60) xor sync_pdata_read;
               end if;
            when pr_CRC16_receiving =>
               -- shifting the CRC16 value to compare it to data received
               if cmd_recv_strobe then
                  CRC16_in <= CRC16_in(59 downto 0) & "0000";
               end if;
            when ps_data_sending =>
               -- the actual CRC16 computation
               if cmd_send_strobe then
                  CRC16_in(63 downto 52) <= CRC16_in(59 downto 48);
                  CRC16_in(51 downto 48) <= CRC16_in(47 downto 44) xor CRC16_in(63 downto 60) xor nible_to_send;
                  CRC16_in(47 downto 24) <= CRC16_in(43 downto 20);
                  CRC16_in(23 downto 20) <= CRC16_in(19 downto 16) xor CRC16_in(63 downto 60) xor nible_to_send;
                  CRC16_in(19 downto  4) <= CRC16_in(15 downto 0);
                  CRC16_in( 3 downto  0) <=                            CRC16_in(63 downto 60) xor nible_to_send;
               end if;
            when ps_CRC16_sending   =>
               -- shifting the CRC16 value to push it to the output reg
               if cmd_send_strobe then
                  CRC16_in <= CRC16_in(59 downto 0) & "0000";
               end if;
            when others => null;
         end case;
      end if;
   end process;


   -- checking CRC
   -- FIXME : could be much simple using the property CRC(data+CRC)=0
   process(clk)
   begin
      if rising_edge(clk) then
         if cmd_recv_strobe then
            if (fsm_packet_reception = pr_CRC16_receiving) then
               -- we set the WrongCRCbit if mismatch during CRC check phase
               if active4l then
                  dblk_CRC16_err <= dblk_CRC16_err or (sync_pdata_read /= CRC16_in(63 downto 60));
               else 
                  dblk_CRC16_err <= dblk_CRC16_err or (sync_pdata_read(0) /= CRC16_in(60));
               end if;
            else
               -- otherwise, we clear it when no operation done
               dblk_CRC16_err <= dblk_CRC16_err and fsm_packet_reception /= p_idle;
            end if;
         end if;
      end if;
   end process;

   -- identifiy nibles incoming
   process(clk)
   begin
      if rising_edge(clk) then
         if (fsm_packet_reception = pr_expecting_start or fsm_packet_reception = ps_start) then
            last_nible <= False;
         elsif (fsm_packet_reception = pr_data_receiving and cmd_recv_strobe) 
            or (fsm_packet_reception = ps_data_sending   and cmd_send_strobe)then
            last_nible <= not last_nible;
         end if;
      end if;
   end process;

   -- identifying the byte to store
   process(clk)
   begin
      if rising_edge(clk) then
         if (fsm_packet_reception = pr_expecting_start) or (fsm_packet_reception = ps_start) then
            byte_cnt <= WORD_SIZE_BYTES - 1;
         elsif (fsm_packet_reception = pr_data_receiving and cmd_recv_strobe and last_nible)
            or (fsm_packet_reception = ps_data_sending   and cmd_send_strobe and last_nible) then
               if byte_cnt = 0 then
                  byte_cnt <= WORD_SIZE_BYTES - 1;
               else
                  byte_cnt <= byte_cnt - 1;
               end if;
         end if;
      end if;
   end process;

   -- preparing the data to carry, data are sent MSB first.
   -- FIXME : this process is only valid for 4 lines transfers. (active4l = True)
   process(clk)
   begin
      if rising_edge(clk) then
         if (fsm_packet_reception = pr_data_receiving) and cmd_recv_strobe then
            -- here, we rebuild a data word from the nibles coming from the SDcard
            if last_nible then
               data_word (WORD_SIZE_BITS - 5 downto WORD_SIZE_BITS - 8) <= sync_pdata_read;
            else
               if WORD_SIZE_BYTES > 1 then
                  data_word(WORD_SIZE_BITS - 9 downto 0) <= data_word(WORD_SIZE_BITS - 1 downto 8);
               end if;
               data_word (WORD_SIZE_BITS - 1 downto WORD_SIZE_BITS - 4) <= sync_pdata_read;
            end if;
         elsif (fsm_packet_reception = ps_data_sending) and cmd_send_strobe then
            -- here, we split data from buffer to send nibles in the proper order
            -- for this part to work, r_data_word must be already loaded ... so buff_addr should more or less anticipate this
            -- see management of 'read_from_buff'
            if last_nible then
               if byte_cnt = 0 then
                  data_word <= r_data_word;
               elsif WORD_SIZE_BYTES > 1  then
                  data_word(WORD_SIZE_BITS - 9 downto 0) <= data_word(WORD_SIZE_BITS - 1 downto 8);
               end if;
            else
               data_word (7 downto 4) <= data_word (3 downto 0);
            end if;
         elsif (fsm_packet_reception = ps_start) and cmd_send_strobe then
            data_word <= r_data_word;
         end if;
      end if;
   end process;

   -- managing read and write bits
   process(clk)
   begin
      if rising_edge(clk) then
         if (fsm_packet_reception = pr_data_receiving) and cmd_recv_strobe and last_nible and
                  (module_state = wait_rcmd17_dblk or module_state = wait_rcmd18_dblk) then
            write_to_buff <= byte_cnt = 0;
         else
            write_to_buff <= False;
         end if;
      end if;
   end process;
   
   process(fsm_packet_reception, cmd_send_strobe, last_nible, byte_cnt)
   begin
      if (fsm_packet_reception = ps_data_sending) and cmd_send_strobe and (not last_nible) then
         read_from_buff <= byte_cnt = WORD_SIZE_BYTES - 1;
      else
         read_from_buff <= False;
      end if;
   end process;
   
   -- counting the address to read/write to
   process(clk)
   begin
      if rising_edge(clk) then
         if    ((module_state = ready or module_state = readm_ready)  and read_block = '1')
            or ((module_state = ready or module_state = writem_ready) and write_block = '1') then
               -- we don't actually store the buffer value, but directly translate it in buffer address
               buff_addr <= to_integer(unsigned(TR_buffer))*(512/WORD_SIZE_BYTES);
         elsif write_to_buff or read_from_buff then
            if pr_data_counter = 0 then
               -- if we reached the end of the buffer, we go back to the begining (There is no other mean to
               -- remember the buffer in case of CRC eror)
               buff_addr <= buff_addr - (512/WORD_SIZE_BYTES) + 1;
            else
               -- normal write, we increase the address for the next read/write
               buff_addr <= buff_addr + 1;
            end if;
         end if;
      end if;
   end process;
   
   
   nible_to_send <= data_word(7 downto 4);
   
   -- data are actually sent at the end of the state...
   process(clk)
   begin
      if rising_edge(clk) then
         if fsm_packet_reception = ps_start and cmd_send_strobe then
            SD_DAT_prep <= "0000";
            SD_DAT_HZ   <= False;
         elsif fsm_packet_reception = ps_data_sending and cmd_send_strobe then
            SD_DAT_prep <= nible_to_send;
            SD_DAT_HZ   <= False;
         elsif fsm_packet_reception = ps_CRC16_sending and cmd_send_strobe then
            SD_DAT_prep <= CRC16_in(63 downto 60);
            SD_DAT_HZ   <= False;
         elsif fsm_packet_reception = ps_stop_bit and cmd_send_strobe then
            SD_DAT_prep <= "1111";
            SD_DAT_HZ   <= False;
         else
            SD_DAT_HZ   <= True;
            --SD_DAT <= "ZZZZ";
         end if;
      end if;
   end process;
   
   
   dat_buffering_slow : if CLK_FREQ_HZ < 200000000 generate
      -- the command is exceptionnally sent on the falling edge of clk. This is definitely not
      -- good practice, but allows 50MHz transfers with a base clock of 100MHz.
      process(clk)
      begin
         if falling_edge(clk) then
            if SD_DAT_HZ then
               SD_DAT <= "ZZZZ";
            else
               SD_DAT <= SD_DAT_prep;
            end if;
         end if;
      end process;
   end generate;

   dat_buffering_fast : if CLK_FREQ_HZ > 199999999 generate
      -- much easier, but that's piece of cake at such frequency...
      process(clk)
      begin
         if rising_edge(clk) then
            if SD_DAT_HZ then
               SD_DAT <= "ZZZZ";
            else
               SD_DAT <= SD_DAT_prep;
            end if;
         end if;
      end process;
   end generate;


   
   
   
   
   
   
   

   ------------------------------------------------------------------------
   -- User interface endianness management
   ------------------------------------------------------------------------
   easy_case_little_endian : if LITTLE_ENDIAN generate
      data_out            <= data_out_litt_endian;
      data_in_litt_endian <= data_in;
   end generate;

   fancy_case_big_endian : if not LITTLE_ENDIAN generate
      -- the following two processes just switch bytes in the corresponding vector
      -- they actually work also if WORD_SIZE_PW2=0 (1 byte transfers)
      process(data_out_litt_endian)
      begin
         for i in 0 to WORD_SIZE_BYTES-1 loop
            data_out(i*8+7 downto i*8) <= data_out_litt_endian(8*(WORD_SIZE_BYTES-i)-1 downto 8*(WORD_SIZE_BYTES-i)-8);
         end loop;
      end process;
   
      process(data_in)
      begin
         for i in 0 to WORD_SIZE_BYTES-1 loop
            data_in_litt_endian(i*8+7 downto i*8) <= data_in(8*(WORD_SIZE_BYTES-i)-1 downto 8*(WORD_SIZE_BYTES-i)-8);
         end loop;
      end process;
   end generate;


end Behavioral;

