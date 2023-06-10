from gettext import install

import matplotlib as matplotlib
import pip

import matplotlib.pyplot as plt
import matplotlib.image as mpimg
from scipy import ndimage

# Load the image
img = mpimg.imread('/Users/gaberiedel/Downloads/base_fld_clip.jpeg')

# Create the x-y graph
x = [-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5]
y = [2, 4, 6, 8, 10, 12, 10, 8, 6, 4, 2]

plt.scatter(x, y)
plt.xlabel('X-axis')
plt.ylabel('Y-axis')
plt.title('X-Y Graph')

# Overlay the graph on the image
plt.imshow(img, extent=[0, 10, 0, 10])

# Display the overlaid image with the graph
plt.show()
