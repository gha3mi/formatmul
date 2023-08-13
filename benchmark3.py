import matplotlib.pyplot as plt
from itertools import cycle
from collections import defaultdict
import numpy as np

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
marker_cycle = cycle(['o', 'o', 's', '^', 'v', 'D', 'p', '*', '+', 'x', '<', '>','|'])
color_cycle = cycle(['black', 'blue', 'green', 'red', 'purple', 'orange', 'brown', 'pink', 'gray', 'gold', 'magenta', 'olive','maroon'])

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
axes[0].legend(loc='upper left', fontsize='small')  # Adjust fontsize as needed
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
axes[1].legend(loc='upper left', fontsize='small')  # Adjust fontsize as needed
axes[1].grid(True)

plt.tight_layout()
plt.savefig('benchmark3.png', dpi=300)  # Save the first set of plots as PNG

# Calculate average values for the 7th column
average_values_col7 = {}
for key, values in organized_data_col7.items():
    y_values = [v[1] for v in values]
    average = np.mean(y_values)
    average_values_col7[key] = average

# Calculate average values for the 8th column
average_values_col8 = {}
for key, values in organized_data_col8.items():
    y_values = [v[1] for v in values]
    average = np.mean(y_values)
    average_values_col8[key] = average

# Find the method with the highest and lowest average elapsed time
max_elapsed_time_method = max(average_values_col7, key=average_values_col7.get)
min_elapsed_time_method = min(average_values_col7, key=average_values_col7.get)

# Find the method with the highest and lowest average performance
max_performance_method = max(average_values_col8, key=average_values_col8.get)
min_performance_method = min(average_values_col8, key=average_values_col8.get)

# Calculate overall average values for the 7th and 8th columns
overall_average_col7 = np.mean(list(average_values_col7.values()))
overall_average_col8 = np.mean(list(average_values_col8.values()))

# Create bar plots for average values
fig, axes = plt.subplots(1, 2, figsize=(15, 6))

# Bar plot for average elapsed time
bar_colors = ['green' if method == min_elapsed_time_method else 'red' if method == max_elapsed_time_method else 'blue' for method in average_values_col7.keys()]
bars = axes[0].bar(average_values_col7.keys(), average_values_col7.values(), color=bar_colors)
# Add overall average value to the bar plot
axes[0].axhline(y=overall_average_col7, color='gray', linestyle='dashed', label='Overall Average')
axes[0].set_xlabel('Methods')
axes[0].set_ylabel('Average Elapsed Time [s]')
axes[0].set_title(f'Coarray Matmul Benchmark - Average elapsed time (number_images={number_images})')
axes[0].tick_params(axis='x', rotation=45)
axes[0].grid(True)

# Bar plot for average performance
bar_colors = ['green' if method == max_performance_method else 'red' if method == min_performance_method else 'blue' for method in average_values_col8.keys()]
bars = axes[1].bar(average_values_col8.keys(), average_values_col8.values(), color=bar_colors)
# Add overall average value to the bar plot
axes[1].axhline(y=overall_average_col8, color='gray', linestyle='dashed', label='Overall Average')
axes[1].set_xlabel('Methods')
axes[1].set_ylabel('Average Performance [GFLOPS]')
axes[1].set_title(f'Coarray Matmul Benchmark - Average performance (number_images={number_images})')
axes[1].tick_params(axis='x', rotation=45)
axes[1].grid(True)

plt.tight_layout()
plt.savefig('benchmark3b.png', dpi=300)  # Save the first set of plots as PNG
plt.show()
