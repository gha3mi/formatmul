# Formatmul
# Seyed Ali Ghasemi
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib as mpl
from matplotlib.ticker import ScalarFormatter

# Define the file name as a variable
file_name = 'benchmark/benchmark3_im1.data'

# Read only the 4th row from the data file to get number of images
with open(file_name, 'r') as file:
    images = file.readlines()[3].split()

# Extract the value from the 4th row for number of images
num_images = images[1]  # Assuming the value is in the second column (index 1)

# Read the benchmark data from the file
benchmark_data = pd.read_csv(file_name, sep=' ', skiprows=4, header=None)

# Assign column names to the benchmark data
benchmark_data.columns = ['method', 'm', 'n', 'o', 'nloops', 'elapsed_time', 'gflops', 'elapsed_time_average', 'gflops_total']

# Calculate the number of elements in the resulting matrix
benchmark_data['num_elements'] = benchmark_data['m'] * benchmark_data['n'] * benchmark_data['o']

# Get unique methods in the dataset for plotting
unique_methods = benchmark_data['method'].unique()

# Define markers and colors automatically for each unique method
markers = {method: plt.Line2D.filled_markers[idx % len(plt.Line2D.filled_markers)]
           for idx, method in enumerate(unique_methods)}

# Generate colors based on the number of unique methods
num_unique_methods = len(unique_methods)
cmap = plt.cm.get_cmap('prism', num_unique_methods)  # Using 'tab20' colormap with the number of unique methods
colors = {method: mpl.colors.rgb2hex(cmap(i)[:3]) for i, method in enumerate(unique_methods)}


# Create the first plot for elapsed_time_average vs size
plt.figure(figsize=(7, 6))  # Create a new figure for the first plot
plt.title(f'Coarray Matmul Benchmark - Average Elapsed Time\nnumber_images={num_images}')  # Title for the plot
for method, group in benchmark_data.groupby('method'):
    plt.plot(group['num_elements'], group['elapsed_time_average'], label=method, marker=markers[method], color=colors[method])
plt.xlabel('Number of Elements in the Resulting Matrix')  # X-axis label
plt.ylabel('Average Elapsed Time [s]')  # Y-axis label
plt.legend(loc='upper left', bbox_to_anchor=(1, 1), ncol=1)  # Legend settings
plt.gca().xaxis.set_major_formatter(ScalarFormatter(useMathText=True))  # X-axis format settings
plt.gca().ticklabel_format(axis='x', style='sci', scilimits=(-2, 2))  # X-axis tick label format
plt.grid(True)  # Enable grid
plt.tight_layout()  # Adjust layout for better visualization

# Save the first plot as an image file
plt.savefig(f'benchmark/benchmark3t_nim{num_images}.png', dpi=300)  # Save the plot as an image

# Create the second plot for gflops_total vs size
plt.figure(figsize=(7, 6))  # Create another new figure for the second plot
plt.title(f'Coarray Matmul Benchmark - Total Performance\nnumber_images={num_images}')  # Title for the plot
for method, group in benchmark_data.groupby('method'):
    plt.plot(group['num_elements'], group['gflops_total'], label=method, marker=markers[method], color=colors[method])
plt.xlabel('Number of Elements in the Resulting Matrix')  # X-axis label
plt.ylabel('Total Performance [GFLOPS]')  # Y-axis label
plt.legend(loc='upper left', bbox_to_anchor=(1, 1), ncol=1)  # Legend settings
plt.gca().xaxis.set_major_formatter(ScalarFormatter(useMathText=True))  # X-axis format settings
plt.gca().ticklabel_format(axis='x', style='sci', scilimits=(-2, 2))  # X-axis tick label format
plt.grid(True)  # Enable grid
plt.tight_layout()  # Adjust layout for better visualization

# Save the second plot as an image file
plt.savefig(f'benchmark/benchmark3p_nim{num_images}.png', dpi=300)  # Save the plot as an image

# Display both plots
plt.show()
