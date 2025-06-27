import pandas as pd


def load_excel(file_path):
    """
    Load an Excel file into a DataFrame.
    """
    return pd.read_excel(file_path)


def find_inserted_rows(original_df, modified_df):
    """
    Identify rows inserted in the modified DataFrame compared to the original DataFrame.
    This function detects empty rows in the modified DataFrame and determines the type of insertion
    based on the number of consecutive empty rows. It then associates the insertion with the closest
    original peaks before and after the empty block, assigning labels according to the specified rules.

    Args:
        original_df (pd.DataFrame): Original DataFrame containing peak information.
        modified_df (pd.DataFrame): Modified DataFrame with potential row insertions.

    Returns:
        list: A list of tuples, where each tuple contains the reference peak and the insertion label
              for each inserted row. The label describes the position relative to the original peaks
              (e.g., "before", "after", "after_after", "before_before").
    """
    inserted_rows = []
    original_peaks = set(original_df['Peak'].dropna())

    index = 0
    while index < len(modified_df):
        peak = modified_df['Peak'][index]

        if pd.isna(peak):  # Empty row in the modified file
            # Collect all consecutive empty rows
            empty_start = index
            while index < len(modified_df) and pd.isna(modified_df['Peak'][index]):
                index += 1
            empty_end = index - 1

            # Determine closest original peaks before and after the block
            before_peak = modified_df['Peak'][empty_start - 1] if empty_start > 0 else None
            after_peak = modified_df['Peak'][index] if index < len(modified_df) else None

            # Ensure both references exist and belong to the original peaks
            if before_peak in original_peaks or after_peak in original_peaks:
                num_inserted_rows = empty_end - empty_start + 1

                if num_inserted_rows == 1:
                    # Single row insertion: report the next row with "before"
                    if after_peak in original_peaks:
                        inserted_rows.append((after_peak, "before"))

                elif num_inserted_rows == 2:
                    # Two rows inserted: first one with "after", second one with "before"
                    if before_peak in original_peaks:
                        inserted_rows.append((before_peak, "after"))
                    if after_peak in original_peaks:
                        inserted_rows.append((after_peak, "before"))

                elif num_inserted_rows == 3:
                    # Three rows inserted: report as "after", "after_after", "before"
                    if before_peak in original_peaks:
                        inserted_rows.append((before_peak, "after"))
                    if before_peak in original_peaks:
                        inserted_rows.append((before_peak, "after_after"))
                    if after_peak in original_peaks:
                        inserted_rows.append((after_peak, "before"))

                elif num_inserted_rows == 4:
                    # Four or more rows inserted: report as "after", "after_after", "before_before", "before"
                    if before_peak in original_peaks:
                        inserted_rows.append((before_peak, "after"))
                    if before_peak in original_peaks:
                        inserted_rows.append((before_peak, "after_after"))
                    if after_peak in original_peaks:
                        inserted_rows.append((after_peak, "before_before"))
                    if after_peak in original_peaks:
                        inserted_rows.append((after_peak, "before"))

                elif num_inserted_rows == 5:
                    # Five rows inserted:
                    # report as "after", "after_after", "after_after_after" "before_before", "before"
                    if before_peak in original_peaks:
                        inserted_rows.append((before_peak, "after"))
                    if before_peak in original_peaks:
                        inserted_rows.append((before_peak, "after_after"))
                    if before_peak in original_peaks:
                        inserted_rows.append((before_peak, "after_after_after"))
                    if after_peak in original_peaks:
                        inserted_rows.append((after_peak, "before_before"))
                    if after_peak in original_peaks:
                        inserted_rows.append((after_peak, "before"))
                elif num_inserted_rows == 6:
                    # Six rows inserted:
                    # report as "after", "after_after", "after_after_after" "before_before_before", "before_before", "before"
                    if before_peak in original_peaks:
                        inserted_rows.append((before_peak, "after"))
                    if before_peak in original_peaks:
                        inserted_rows.append((before_peak, "after_after"))
                    if before_peak in original_peaks:
                        inserted_rows.append((before_peak, "after_after_after"))
                    if after_peak in original_peaks:
                        inserted_rows.append((after_peak, "before_before_before"))
                    if after_peak in original_peaks:
                        inserted_rows.append((after_peak, "before_before"))
                    if after_peak in original_peaks:
                        inserted_rows.append((after_peak, "before"))

        index += 1

    return inserted_rows


def update_modified_df_with_inserted_rows(modified_df, inserted_rows):
    """
    Update the modified DataFrame by renaming empty rows using the output of the find_inserted_rows function.

    Args:
        modified_df (pd.DataFrame): The modified DataFrame to update.
        inserted_rows (list): List of tuples containing the row positions and directions ('before' or 'after') for insertion.

    Returns:
        pd.DataFrame: The updated modified DataFrame.
    """
    empty_rows = modified_df[modified_df['Peak'].isna()].index  # Get indices of empty rows

    if len(empty_rows) < len(inserted_rows):
        raise ValueError(
            f"Not enough empty rows in the modified DataFrame to apply all inserted rows. "
            f"Found {len(empty_rows)} empty rows, but {len(inserted_rows)} inserted rows."
        )

    # Replace the empty rows in modified_df with names from inserted_rows
    for i, (position_reference, direction) in enumerate(inserted_rows):
        new_row_name = f"{direction}_{position_reference}"
        modified_df.at[empty_rows[i], 'Peak'] = new_row_name

    return modified_df


def find_peak_movements(original_df, modified_df):
    """
    Identify peak movements in the modified DataFrame compared to the original DataFrame.
    This function compares the mapping of retention times to peaks for each sample column.

    Args:
        original_df (pd.DataFrame): Original DataFrame containing peak and sample data.
        modified_df (pd.DataFrame): Modified DataFrame with updated peak information.

    Returns:
        list: A list of tuples containing (sample, original_peak, new_row).
    """
    movements = []  # Initialize a list to store movements
    samples = original_df.columns[6:]  # Select columns representing sample data

    for sample in samples:
        # Create mappings of retention times (rt) to peaks for both original and modified DataFrames
        original_mapping = {rt: peak for peak, rt in zip(original_df['Peak'], original_df[sample]) if not pd.isna(rt)}
        modified_mapping = {rt: peak for peak, rt in zip(modified_df['Peak'], modified_df[sample]) if not pd.isna(rt)}

        for rt, original_peak in original_mapping.items():
            if rt in modified_mapping:
                modified_peak = modified_mapping[rt]
                if original_peak != modified_peak:
                    movements.append((sample, original_peak, modified_peak))
            else:
                movements.append((sample, original_peak, "nan"))

    return movements


def generate_r_script(inserted_rows, peak_movements):
    """
    Generate the R script blocks for row insertions and peak movements.
    """
    # Generate row insertions block
    insertion_block = "master_table <- master_table |> \n  add_empty_peaks2({tribble(~position.reference, ~direction,\n"
    for position_reference, direction in inserted_rows:
        insertion_block += f'                            "{position_reference}", "{direction}",\n'
    insertion_block = insertion_block.strip(',\n') + "\n  )})\n"

    # Generate peak movements block
    movement_block = "master_table <- master_table |> \n  lapply(shift_rows2\n         , shifts_df = {tribble(~cols_to_shift, ~rows_to_shift, ~new_rows,\n"
    for sample, rows_to_shift, new_rows in peak_movements:
        movement_block += f'                                "{sample}", "{rows_to_shift}", "{new_rows}",\n'
    movement_block = movement_block.strip(',\n') + "\n         )})\n"

    return insertion_block, movement_block


def write_output(file_path, insertion_block, movement_block):
    """Write the R script blocks to a .txt file."""
    with open(file_path, 'w') as f:
        f.write(insertion_block)
        f.write("\n\n")
        f.write(movement_block)


# USAGE ################################################################################################################

if __name__ == "__main__":
    # Change the names of the files here!
    original_file = "original_file_05.xlsx"
    modified_file = "modified_file_jan28-2.xlsx"
    output_file = "test_jan30.txt"

    original_df = load_excel(original_file)
    modified_df = load_excel(modified_file)

    inserted_rows = find_inserted_rows(original_df, modified_df)

    # Update the modified_df based on the inserted rows
    modified_df = update_modified_df_with_inserted_rows(modified_df, inserted_rows)

    peak_movements = find_peak_movements(original_df, modified_df)

    insertion_block, movement_block = generate_r_script(inserted_rows, peak_movements)
    write_output(output_file, insertion_block, movement_block)