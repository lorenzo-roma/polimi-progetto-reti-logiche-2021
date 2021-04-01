library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity mem_scanner is
    port(
        i_clk       : in std_logic;
        i_rst       : in std_logic;
        i_start     : in std_logic;
        i_mem_data      : in std_logic_vector (7 downto 0);
        o_value     : out std_logic_vector(7 downto 0);
        o_read_enable: out std_logic;
        o_mem_address   : out std_logic_vector(15 downto 0);
        o_mem_enable       : out std_logic;
        o_done      : out std_logic
    );
end mem_scanner;

architecture Behavioral of mem_scanner is
subtype byte is std_logic_vector(7 downto 0);
type state is (
    state_idle, 
    state_start, 
    state_wait_rows, 
    state_read_rows, 
    state_wait_columns, 
    state_read_columns, 
    state_calculate_pixels,
    state_wait_write,
    state_wait_pixel, 
    state_read_pixel,state_done
);

signal n_rows       : byte;
signal n_columns    : byte;
signal n_pixels      : unsigned(15 downto 0);
signal n_pixel_remains : unsigned(15 downto 0);
signal mem_pointer : unsigned(15 downto 0);
signal state_current, state_next    : state;


    
begin
    
    state_switcher : 
    process(i_clk, i_rst) begin
        if(i_rst='1') then state_current <= state_idle;
        elsif rising_edge(i_clk) then
            state_current <= state_next;
        end if;
    end process state_switcher;
    
    state_applier : 
    process(state_current, i_start) begin
            o_read_enable <= '0';
            o_mem_address <= "0000000000000000";
            state_next <= state_current;
            o_mem_enable <= '0';
            o_done <= '0';
            o_value <= "00000000";
            
            case state_current is
                when state_idle =>
                    if(i_start='1') then state_next <= state_start; end if;
                when state_start =>
                    o_mem_enable <= '1';
                    state_next <= state_wait_rows;
                when state_wait_rows =>
                    o_mem_enable <= '1';
                    state_next <= state_read_rows;
                when state_read_rows =>
                    o_mem_enable <= '1';
                    n_rows <= i_mem_data;
                    o_value <= i_mem_data;
                    o_mem_address <= "0000000000000001";
                    state_next <= state_wait_columns;
                when state_wait_columns =>
                    o_mem_enable <= '1';
                    o_mem_address <= "0000000000000001";
                    state_next <= state_read_columns;
                when state_read_columns =>
                    o_mem_address <= "0000000000000001";
                    mem_pointer <= to_unsigned(0, 16);
                    n_columns <= i_mem_data;
                    o_value <= i_mem_data;
                    o_mem_enable <= '1';
                    state_next <= state_calculate_pixels;
                when state_calculate_pixels =>
                    n_pixels <= unsigned(n_rows) * unsigned(n_columns);
                    n_pixel_remains <= unsigned(n_rows) * unsigned(n_columns);
                    state_next <= state_wait_pixel;
                    o_mem_enable <= '1';
                    o_mem_address <= std_logic_vector(mem_pointer  + to_unsigned(2, 16));
                when state_wait_pixel =>
                    if(n_pixel_remains > 0 )then
                        o_mem_enable <= '1';
                        o_mem_address <= std_logic_vector(mem_pointer  + to_unsigned(2, 16));
                        state_next <= state_read_pixel;
                    else 
                        state_next <= state_done;
                    end if;
                when state_read_pixel =>
                    mem_pointer <= unsigned(mem_pointer + to_unsigned(1, 16));
                    n_pixel_remains <= unsigned(n_pixel_remains - to_unsigned(1, 16));
                    o_value <= i_mem_data;
                    o_read_enable <= '1';
                    state_next <= state_wait_write;
                    o_mem_enable <= '1';
                    o_mem_address <= std_logic_vector(mem_pointer  + to_unsigned(2, 16) + n_pixels);
                when state_wait_write =>
                    o_value <= "11111111";
                    o_mem_enable <= '1';
                    o_mem_address <= std_logic_vector(mem_pointer  + to_unsigned(2, 16));
                    state_next <= state_wait_pixel;
                when state_done =>
                    o_done <= '1';
                    state_next <= state_idle;
            end case;
    end process state_applier;


end;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity max_min_calculator is
    port(
        i_clk: in std_logic;
        i_rst: in std_logic;
        i_enable_read: in std_logic;
        i_value: in std_logic_vector(7 downto 0);
        i_finish_scan: in std_logic;
        o_done: out std_logic;
        o_min: out std_logic_vector(7 downto 0);
        o_max: out std_logic_vector(7 downto 0)
    );
end max_min_calculator;

architecture Behavioural of max_min_calculator is 
    subtype byte is std_logic_vector(7 downto 0);
    type state is (
        state_idle, 
        state_update_values, 
        state_wait_values,
        state_done
    );
    
    signal min_value    : byte;
    signal max_value    : byte;
    signal state_current    : state;


begin

    state_switcher : 
    process(i_clk, i_rst, i_enable_read, i_finish_scan) begin
        if(i_rst='1') then state_current <= state_idle;
        elsif rising_edge(i_clk) then
            if(not(state_current = state_done)) then
                    if(i_enable_read='1') then state_current <= state_update_values; end if;
                    if(i_finish_scan='1') then state_current <= state_done;
                    elsif(i_enable_read='0') then state_current <= state_wait_values; end if;
            end if;
        end if;
    end process state_switcher;
    
    state_applier : 
    process(state_current) begin
        o_done <= '0';
        o_min <= "00000000";
        o_max <= "00000000";
        case state_current is
            when state_idle =>
                min_value  <= "11111111";
                max_value <= "00000000";
           when state_update_values =>
                if(unsigned(min_value)>unsigned(i_value)) then min_value <= i_value; end if;
                if(unsigned(max_value)<unsigned(i_value)) then max_value <= i_value; end if;
           when state_wait_values =>
           when state_done =>
                o_done <= '1';
                o_min <= min_value;
                o_max <= max_value;
        end case;
    end process state_applier;


end;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity equalizer is
    port(
        i_clk: in std_logic;
        i_rst: in std_logic;
        i_start: in std_logic;
        i_min_value: in std_logic_vector(7 downto 0);
        i_max_value: in std_logic_vector(7 downto 0);
        i_value: in std_logic_vector(7 downto 0);
        i_enable_read: in std_logic;
        i_finish_scan : in std_logic;
        o_mem_start_scan: out std_logic;
        o_new_pixel_value: out std_logic_vector(7 downto 0);
        o_value_readable: out std_logic;
        o_done: out std_logic
    );
end equalizer;

architecture Behavioural of equalizer is 
    subtype byte is std_logic_vector(7 downto 0);
    type state is (
        state_idle,
        state_start_scan,
        state_equalize_pixel,
        state_done
    );
    signal state_current, state_next: state;
    signal delta_value: integer;
    signal temp_pixel: unsigned(15 downto 0);
begin
   
    state_switcher : 
    process(i_clk, i_rst) begin
        if(i_rst='1') then state_current <= state_idle;
        elsif rising_edge(i_clk) then
            state_current <= state_next;
        end if;
    end process state_switcher;
    
    state_applier : 
    process(state_current, i_start, i_enable_read, i_finish_scan) begin
            o_done <= '0';
            o_value_readable <= '0';
            o_mem_start_scan <= '0';
            state_next <= state_current;
            o_new_pixel_value <= "00000000";
            case state_current is
                when state_idle =>
                    if(i_start='1') then state_next <= state_start_scan; end if;
                when state_start_scan =>
                   delta_value <= to_integer((unsigned(i_max_value)) - (unsigned(i_min_value)));
                   o_mem_start_scan <= '1';
                   state_next <= state_equalize_pixel;
                when state_equalize_pixel =>
                    if(i_finish_scan='1') then 
                        state_next <= state_done;
                    else
                        if(i_enable_read='1') then 
                        o_value_readable <= '1';
                        case delta_value is
                            when 255 =>
                                o_new_pixel_value <= std_logic_vector(shift_left(unsigned(i_value) - unsigned(i_min_value), 0));
                            when 127 to 254 =>  
                                if(unsigned(i_value) - unsigned(i_min_value) > to_unsigned(127, 8)) then
                                    o_new_pixel_value <= "11111111";
                                else
                                    o_new_pixel_value <= std_logic_vector(shift_left((unsigned(i_value) - unsigned(i_min_value)), 1));
                                end if;                                  
                            when 63 to 126 =>
                                if(unsigned(i_value) - unsigned(i_min_value) > to_unsigned(63, 8)) then
                                    o_new_pixel_value <= "11111111";
                                else
                                    o_new_pixel_value <= std_logic_vector(shift_left((unsigned(i_value) - unsigned(i_min_value)), 2));
                                end if;                                      
                            when 31 to 62 =>
                                if(unsigned(i_value) - unsigned(i_min_value) > to_unsigned(31, 8)) then
                                    o_new_pixel_value <= "11111111";
                                else
                                    o_new_pixel_value <= std_logic_vector(shift_left((unsigned(i_value) - unsigned(i_min_value)), 3));    
                                end if;                                   
                            when 15 to 30 =>                                    
                                 if(unsigned(i_value) - unsigned(i_min_value) > to_unsigned(15, 8)) then
                                    o_new_pixel_value <= "11111111";
                                else
                                    o_new_pixel_value <= std_logic_vector(shift_left( (unsigned(i_value) - unsigned(i_min_value)), 4));    
                                end if;                               
                            when 7 to 14 =>                                    
                                 if(unsigned(i_value) - unsigned(i_min_value) > to_unsigned(7, 8)) then
                                    o_new_pixel_value <= "11111111";
                                else
                                    o_new_pixel_value <= std_logic_vector(shift_left( (unsigned(i_value) - unsigned(i_min_value)), 5));
                                end if;
                            when 3 to 6 =>                                    
                                 if(unsigned(i_value) - unsigned(i_min_value) > to_unsigned(3, 8)) then
                                    o_new_pixel_value <= "11111111";
                                else
                                    o_new_pixel_value <= std_logic_vector(shift_left( (unsigned(i_value) - unsigned(i_min_value)), 6));
                                end if;
                            when 1 to 2 =>                                    
                                 if(unsigned(i_value) - unsigned(i_min_value) > to_unsigned(3, 8)) then
                                    o_new_pixel_value <= "11111111";
                                else
                                    o_new_pixel_value <= std_logic_vector(shift_left( (unsigned(i_value) - unsigned(i_min_value)), 7));
                                end if;
                            when 0 =>                                    
                                 if(unsigned(i_value) - unsigned(i_min_value) > to_unsigned(0, 8)) then
                                    o_new_pixel_value <= "11111111";
                                else
                                    o_new_pixel_value <= std_logic_vector(shift_left( (unsigned(i_value) - unsigned(i_min_value)), 8));
                                end if;
                            when others =>
                        end case;
                     state_next <= state_equalize_pixel;
                    end if;
                    end if;
                when state_done =>
                    o_done <= '1';
            end case;
    end process state_applier;



end;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity multiplexer is
    port(
        i_first: in std_logic;
        i_second: in std_logic;
        i_control: in std_logic;
        o_value: out std_logic
    );
end multiplexer;

architecture Behavioral of multiplexer is
begin
    o_value <= i_second when (i_control = '1') else i_first;
end Behavioral;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity project_reti_logiche is
port (
i_clk : in std_logic;
i_rst : in std_logic;
i_start : in std_logic;
i_data : in std_logic_vector(7 downto 0);
o_address : out std_logic_vector(15 downto 0);
o_done : out std_logic;
o_en : out std_logic;
o_we : out std_logic;
o_data : out std_logic_vector (7 downto 0)
);
end project_reti_logiche;

architecture Behavioral of project_reti_logiche is
signal   output_from_scanner    : std_logic_vector (7 downto 0);
signal   enable_read_scanner    : std_logic;
signal   finish_scanner         : std_logic;
signal   min_calculated         : std_logic_vector(7 downto 0);
signal   max_calculated         : std_logic_vector(7 downto 0);
signal   finish_first_scan      : std_logic;
signal mem_start_scanner        : std_logic;
signal start_mem                : std_logic;



    component mem_scanner is 
     port(
        i_clk       : in std_logic;
        i_rst       : in std_logic;
        i_start     : in std_logic;
        i_mem_data      : in std_logic_vector (7 downto 0);
        o_value     : out std_logic_vector(7 downto 0);
        o_read_enable : out std_logic;
        o_mem_address   : out std_logic_vector(15 downto 0);
        o_mem_enable       : out std_logic;
        o_done      : out std_logic
    );
    end component;
    

component max_min_calculator is
    port(
        i_clk: in std_logic;
        i_rst: in std_logic;
        i_enable_read: in std_logic;
        i_value: in std_logic_vector(7 downto 0);
        i_finish_scan: in std_logic;
        o_done: out std_logic;
        o_min: out std_logic_vector(7 downto 0);
        o_max: out std_logic_vector(7 downto 0)
    );
end component;

component equalizer is
        port(
        i_clk: in std_logic;
        i_rst: in std_logic;
        i_start: in std_logic;
        i_min_value: in std_logic_vector(7 downto 0);
        i_max_value: in std_logic_vector(7 downto 0);
        i_value: in std_logic_vector(7 downto 0);
        i_enable_read: in std_logic;
        i_finish_scan : in std_logic;
        o_mem_start_scan: out std_logic;
        o_new_pixel_value: out std_logic_vector(7 downto 0);
        o_value_readable: out std_logic;
        o_done: out std_logic
    );
end component;

component multiplexer is
    port(
        i_first: in std_logic;
        i_second: in std_logic;
        i_control: in std_logic;
        o_value: out std_logic
    );
end component;

begin

    MULTIPLEXER_TESTED:
    multiplexer port map(
        i_first => i_start,
        i_second => mem_start_scanner,
        i_control => finish_first_scan,
        o_value => start_mem
    );

    CALCULATOR_TESTED: 
    max_min_calculator port map(
        i_clk => i_clk,
        i_rst => i_rst,
        i_enable_read => enable_read_scanner,
        i_value => output_from_scanner,
        i_finish_scan => finish_scanner,
        o_done => finish_first_scan,
        o_min => min_calculated,
        o_max => max_calculated
    );
    
    MEM_SCANNER_TESTED: 
    mem_scanner port map(
        i_clk           => i_clk,
        i_rst           => i_rst,
        i_start         => start_mem,
        i_mem_data      => i_data,
        o_value         => output_from_scanner,
        o_read_enable   => enable_read_scanner,
        o_mem_address   => o_address,
        o_mem_enable    => o_en,
        o_done          => finish_scanner
    );
    
    EQUALIZER_TESTED:
    equalizer port map(
        i_clk           => i_clk,
        i_rst           => i_rst,
        i_start         => finish_first_scan,
        i_min_value     => min_calculated,
        i_max_value     => max_calculated,
        i_value         => output_from_scanner,
        i_enable_read   => enable_read_scanner,
        i_finish_scan   => finish_scanner,
        o_mem_start_scan    => mem_start_scanner,
        o_new_pixel_value   => o_data,
        o_value_readable    => o_we,
        o_done       => o_done
    );




end Behavioral;
