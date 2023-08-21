# Formatmul
# Seyed Ali Ghasemi
import matplotlib.pyplot as plt
from matplotlib.ticker import ScalarFormatter
from itertools import cycle
from collections import defaultdict
import numpy as np

# Read benchmark data from a file
data = []
with open('benchmark/benchmark4_im1.data', 'r') as file:
    for _ in range(3):
        next(file)  # Skip the first 3 rows
    constant_value_row = next(file).strip().split()  # Read the constant value row
    number_images = int(constant_value_row[1])  # Extract the number of images
    for line in file:
        values = line.strip().split()
        identifier = values[0]
        x_value = int(values[1])*int(values[3])  # m*o
        elapsed_time = float(values[7])  # Extract elapsed time (y-value for elapsed time plot)
        performance = float(values[8])  # Extract performance (y-value for performance plot)
        data.append((identifier, x_value, elapsed_time, performance))

# Organize data based on the identifier
organized_data_elapsed_time = defaultdict(list)
organized_data_performance = defaultdict(list)
for row in data:
    identifier = row[0]
    organized_data_elapsed_time[identifier].append((row[1], row[2]))
    organized_data_performance[identifier].append((row[1], row[3]))

# Create a figure with two subplots
fig, axes = plt.subplots(1, 2, figsize=(15, 6))

# Assign unique markers and colors for each identifier
identifier_marker_color = {}
marker_cycle = cycle(['o','.', 'o', 's', '^', 'v', 'D', 'p', '*', '+', 'x', '<', '>','|'])
color_cycle = cycle(['teal','black', 'blue', 'green', 'red', 'purple', 'orange', 'brown', 'pink', 'gray', 'gold', 'magenta', 'olive','maroon'])

# Plot elapsed time data
for key, values in organized_data_elapsed_time.items():
    x_values = [v[0] for v in values]
    elapsed_time_values = [v[1] for v in values]
    marker = next(marker_cycle)
    color = next(color_cycle)
    identifier_marker_color[key] = (marker, color)

    axes[0].plot(x_values, elapsed_time_values, marker=marker, color=color, linestyle='-', label=key)

# Configure and show the first subplot
axes[0].set_title(f'Coarray Matmul Benchmark - Average Elapsed Time\nnumber_images={number_images}')
axes[0].set_xlabel('Number of Elements in the Resulting Matrix')
axes[0].set_ylabel('Average Elapsed Time [s]')
axes[0].legend(loc='upper left', fontsize='small')
axes[0].grid(True)
axes[0].xaxis.set_major_formatter(ScalarFormatter(useMathText=True))
axes[0].ticklabel_format(axis='x', style='sci', scilimits=(-2,2))

# Plot performance data
for key, values in organized_data_performance.items():
    x_values = [v[0] for v in values]
    performance_values = [v[1] for v in values]
    
    marker, color = identifier_marker_color[key]
    
    axes[1].plot(x_values, performance_values, marker=marker, color=color, linestyle='-', label=key)

# Configure and show the second subplot
axes[1].set_title(f'Coarray Matmul Benchmark - Total Performance\nnumber_images={number_images}')
axes[1].set_xlabel('Number of Elements in the Resulting Matrix')
axes[1].set_ylabel('Total Performance [GFLOPS]')
axes[1].legend(loc='upper left', fontsize='small')
axes[1].grid(True)
axes[1].xaxis.set_major_formatter(ScalarFormatter(useMathText=True))
axes[1].ticklabel_format(axis='x', style='sci', scilimits=(-2,2))

# Adjust layout, save the first set of plots, and display
plt.tight_layout()
plt.savefig(f'benchmark/benchmark4a_nim{number_images}.png', dpi=300)
plt.show()

# Calculate average values for elapsed time and performance
average_values_elapsed_time = {}
average_values_performance = {}
for key, values in organized_data_elapsed_time.items():
    elapsed_time_values = [v[1] for v in values]
    average_elapsed_time = np.mean(elapsed_time_values)
    average_values_elapsed_time[key] = average_elapsed_time
    
for key, values in organized_data_performance.items():
    performance_values = [v[1] for v in values]
    average_performance = np.mean(performance_values)
    average_values_performance[key] = average_performance

# Find methods with the highest and lowest average elapsed time and performance
max_elapsed_time_method = max(average_values_elapsed_time, key=average_values_elapsed_time.get)
min_elapsed_time_method = min(average_values_elapsed_time, key=average_values_elapsed_time.get)

max_performance_method = max(average_values_performance, key=average_values_performance.get)
min_performance_method = min(average_values_performance, key=average_values_performance.get)

# Calculate overall average values for elapsed time and performance
overall_average_elapsed_time = np.mean(list(average_values_elapsed_time.values()))
overall_average_performance = np.mean(list(average_values_performance.values()))

# Create bar plots for average values
fig, axes = plt.subplots(1, 2, figsize=(15, 6))

# Bar plot for average elapsed time
bar_colors_elapsed_time = ['green' if method == min_elapsed_time_method else 'red' if method == max_elapsed_time_method else 'blue' for method in average_values_elapsed_time.keys()]
bars = axes[0].bar(average_values_elapsed_time.keys(), average_values_elapsed_time.values(), color=bar_colors_elapsed_time)

# Add overall average value to the bar plot
axes[0].axhline(y=overall_average_elapsed_time, color='gray', linestyle='dashed', label='Overall Average')
axes[0].set_title(f'Coarray Matmul Benchmark - Average Elapsed Time\nnumber_images={number_images}')
axes[0].set_xlabel('Methods')
axes[0].set_ylabel('Average Elapsed Time [s]')
axes[0].tick_params(axis='x', rotation=45)
axes[0].grid(True)

# Bar plot for average performance
bar_colors_performance = ['green' if method == max_performance_method else 'red' if method == min_performance_method else 'blue' for method in average_values_performance.keys()]
bars = axes[1].bar(average_values_performance.keys(), average_values_performance.values(), color=bar_colors_performance)

# Add overall average value to the bar plot
axes[1].axhline(y=overall_average_performance, color='gray', linestyle='dashed', label='Overall Average')
axes[1].set_title(f'Coarray Matmul Benchmark - Average Total Performance\nnumber_images={number_images}')
axes[1].set_xlabel('Methods')
axes[1].set_ylabel('Average Total Performance [GFLOPS]')
axes[1].tick_params(axis='x', rotation=45)
axes[1].grid(True)

# Adjust layout, save the second set of plots, and display
plt.tight_layout()
plt.savefig(f'benchmark/benchmark4b_nim{number_images}.png', dpi=300)
plt.show()
