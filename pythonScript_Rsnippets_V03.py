"""
DATE: 20240815
AUTHORS: Caro, Max and ChatGPT
VERSION: 03
SCRIPT_NAME: pythonScript_Rsnippets
"""


def parse_input(input_text):
    lines = input_text.strip().splitlines()
    header = lines[0].split(":")[0].strip()

    peaks_movements = []
    for line in lines[1:]:
        if ": " in line:
            peak, movement = line.split(": ")
            peak = peak.strip()
            movement = movement.strip().lower()
            if movement.startswith('upx') or movement.startswith('downx'):
                peaks_movements.append((peak, movement))
            else:
                print(f"Warning: Invalid movement format '{movement}' for peak '{peak}'. Skipping.")

    return header, peaks_movements


def generate_movement_dataframes(header, peaks_movements):
    movement_dict = {'upx': -1, 'downx': 1}
    dataframes = []

    def get_movement_count(movement, peak):
        # Identify the prefix dynamically
        for prefix in movement_dict.keys():
            if movement.startswith(prefix):
                count_str = movement[len(prefix):].strip()  # Extract count part after prefix
                try:
                    return int(count_str)
                except ValueError:
                    print(f"Error: Invalid movement count in '{movement}' for peak '{peak}'. Skipping.")
                    return 0
        print(f"Error: Movement string '{movement}' does not start with a valid prefix for peak '{peak}'. Skipping.")
        return 0

    def get_movement_direction(movement, peak):
        for prefix in movement_dict.keys():
            if movement.startswith(prefix):
                return 'up' if prefix == 'upx' else 'down'
        print(f"Warning: Invalid movement direction '{movement}' for peak '{peak}'. Skipping.")
        return None

        # Process current peaks and movements

    current_peaks = [peak for peak, _ in peaks_movements]
    current_movements = [get_movement_direction(movement, peak) for peak, movement in peaks_movements]
    current_movements = [mov for mov in current_movements if mov]  # Filter out None values

    dataframes.append((current_peaks, current_movements))

    max_movement = max(get_movement_count(movement, peak) for peak, movement in peaks_movements)

    for step in range(1, max_movement):
        new_peaks = []
        new_movements = []
        for peak, movement in peaks_movements:
            # Determine the correct prefix length based on the available keys in movement_dict
            prefix = next((key for key in movement_dict if movement.startswith(key)), None)

            if prefix:
                direction = movement_dict[prefix]
                count = get_movement_count(movement, peak)
                if step < count:
                    peak_number = int(peak[1:]) + direction * step
                    new_peaks.append(f"P{peak_number:03}")
                    new_movements.append('up' if 'up' in movement else 'down')
            else:
                print(f"Error: Invalid movement direction in '{movement}' for peak '{peak}'. Skipping.")
                continue

        if new_peaks:
            dataframes.append((new_peaks, new_movements))

    return dataframes


def prompt_input():
    print(f"______INPUT_TEXT______"
          f"{input_text}"
          f"________R_code________")
    return


def print_r_code(header, dataframes):
    for i, (peaks, movements) in enumerate(dataframes, start=1):
        peaks_list = ','.join(f'\"{peak}\"' for peak in peaks)
        movements_list = ','.join(f'\"{movement}\"' for movement in movements)
        print(
            f"peaks_movements{i} <- list( \"{header}\" = data.frame(peaks_list = c({peaks_list}) , movement_dirs = c({movements_list})))")

    print("\ncorrected_samples_data_list <- aligned_samples_data_list |>")
    for i in range(1, len(dataframes) + 1):
        print(f"  lapply(correct_alignment, movements_list = peaks_movements{i}) |>")
    print("  lapply(function(l) {{")
    print("    l |> ")
    print("      lapply(function(df) {{")
    print("        df |> ")
    print("          t() |> as.data.frame()")
    print("      }})")
    print("  }})")


# Input text from user
input_text = """  
234:
P179: Upx1
"""

# Parsing input and generating dataframes
header, peaks_movements = parse_input(input_text)
dataframes = generate_movement_dataframes(header, peaks_movements)

# Print R code
prompt_input()
print_r_code(header, dataframes)