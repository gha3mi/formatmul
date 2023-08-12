import matplotlib.pyplot as plt
from itertools import cycle
from collections import defaultdict

# Read the data from the file
data = []
with open('benchmark3', 'r') as file:
    next(file)  # Skip the first line
    second_row = next(file).strip().split()  # Read the second row
    number_images = second_row[1]  # Use the second column of the second row
    for line in file:
        values = line.strip().split()
        identifier = values[0]
        x_value = int(values[2])  # Use the 3rd column as x-value
        y_value_col7 = float(values[6])  # Use the 7th column as y-value for the first plot
        y_value_col8 = float(values[7])  # Use the 8th column as y-value for the second plot
        data.append((identifier, x_value, y_value_col7, y_value_col8))

# Organize the data based on the identifier
organized_data_col7 = defaultdict(list)
organized_data_col8 = defaultdict(list)
for row in data:
    identifier = row[0]
    organized_data_col7[identifier].append((row[1], row[2]))
    organized_data_col8[identifier].append((row[1], row[3]))

# Create a subplot with two plots
fig, axes = plt.subplots(1, 2, figsize=(15, 6))

# Use the same marker and color for each identifier
identifier_marker_color = {}  # Dictionary to store marker and color for each identifier

# Cycle through different markers and colors
marker_cycle = cycle(['o', 'o', 's', '^', 'v', 'D', 'p', '*', '+', 'x', '<', '>'])
color_cycle = cycle(['black', 'blue', 'green', 'red', 'purple', 'orange', 'brown', 'pink', 'gray', 'gold', 'magenta', 'olive'])

# Plot for the 7th column as y-value
for key, values in organized_data_col7.items():
    x_values = [v[0] for v in values]
    y_values = [v[1] for v in values]
    
    # Get or assign marker and color for the identifier
    if key in identifier_marker_color:
        marker, color = identifier_marker_color[key]
    else:
        marker = next(marker_cycle)
        color = next(color_cycle)
        identifier_marker_color[key] = (marker, color)
    
    axes[0].plot(x_values, y_values, marker=marker, color=color, linestyle='-', label=key)

axes[0].set_xlabel('m=n=o')
axes[0].set_ylabel('elapsed time [s]')
axes[0].set_title(f'Coarray Matmul Benchmark - Elapsed time (number_images={number_images})')
axes[0].legend()
axes[0].grid(True)

# Plot for the 8th column as y-value
for key, values in organized_data_col8.items():
    x_values = [v[0] for v in values]
    y_values = [v[1] for v in values]
    
    # Use the same marker and color for the identifier
    marker, color = identifier_marker_color[key]
    
    axes[1].plot(x_values, y_values, marker=marker, color=color, linestyle='-', label=key)

axes[1].set_xlabel('m=n=o')
axes[1].set_ylabel('performance [GFLOPS]')
axes[1].set_title(f'Coarray Matmul Benchmark - Performance (number_images={number_images})')
axes[1].legend()
axes[1].grid(True)

plt.tight_layout()
plt.show()
