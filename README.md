# two-danode
Two Dimenisonal Autonomous Non-Linear Differential Equations Visualizer

See the book [Nonlinear Dynamics and Chaos](http://www.stevenstrogatz.com/books/nonlinear-dynamics-and-chaos-with-applications-to-physics-biology-chemistry-and-engineering) by Steven H. Strogatz

Amazing lectures by Prof. Strogatz on YouTube are [here](https://www.youtube.com/playlist?list=PLbN57C5Zdl6j_qJA-pARJnKsmROzPnO9V)

![alt text](https://farm5.staticflickr.com/4704/25792673807_32edd52ff5_z_d.jpg "Example")

## Description
This is a program that can take two functions F (x,y) and G (x,y) from the command line, representing F => &#7819; and G => &#7823; (The change in x and y respectively), and represent the resulting vectors in the [HSL color space](https://en.wikipedia.org/wiki/HSL_and_HSV). 

This isn't scientific, I thought it might be semi intuitive/cool to see the direction of vectors represented by the hue (the 'H' in HSL), such that unstable/stable nodes would be surrounded by a full rainbow. 

Vectors going right are red (0&#176;), then going counter-clockwise on the rainbow, yellow is 60&#176;, teal is 180&#176;, blue is 240&#176;, mageneta is 300&#176; and then back to red at 360&#176;. Magnitude of the vectors are represented by the intensity of the color up to white where the lightness (the 'L' in HSL) is 1 (normalized by the largest magnitude vector).

This can be illustrated with &#7819; = x and &#7823; = y, resulting in

![alt text](https://farm5.staticflickr.com/4746/39953334044_ba1714ba5b_m_d.jpg "Example")

Where right is red, left is teal and the origin is black (zero magnitude, 'L' = 0)
To get this image you can run 

```stack exec -- two-danode-exe -F "x" -G "y" -f ~/Desktop/cartesian.bmp``` 

or in polar coordinates 

```stack exec -- two-danode-exe -P -R "r" -T "0" -f ~/Desktop/polar.bmp``` 

with R and T a function of variables "r" and "theta".

## Installation and Usage
Install [Haskell Stack](https://docs.haskellstack.org/en/stable/README/), cd into directory ```stack install```

You have to run the binary with ```stack exec -- two-danode-exe```, this is because to eval command line arguments in Haskell I use the hint package which needs the environment variables for the location of the installed packages to interpret the equations and return the vector field.

See ```stack exec -- two-danode-exe -h``` for flags and usage. 

## Example 1
![alt text](https://farm5.staticflickr.com/4656/39953332894_1c5c8caf2b_k_d.jpg "Example")
```time stack exec -- two-danode-exe -H 5 -F "(1.5*sin (3*x*y -10))*(abs $ cos (1.75*x))**(abs $ 3 * cos (3*y)) + sin (3*x)" -G "sin (4.3*x) - cos (0.5*y)" -r "(4096,4096)"```

## Example 2
![alt text](https://farm5.staticflickr.com/4704/25792673807_a300fe656f_k_d.jpg "Example")
```time stack exec -- two-danode-exe -H 5 -r "(4096,4096)" -f 051.bmp -F "sin(2*y - 3*x)*exp(-abs (sin (3*x+1-2*y) - cos(x-3*y+1)))" -G "cos(2*y - 3*x)*exp(-abs (cos (3*x+1-2*y) - cos(x-3*y+1)))"```

## Example 3
![alt text](https://farm5.staticflickr.com/4611/25792677967_1afd9e8d9b_k_d.jpg "Example")
```time stack exec -- two-danode-exe -F "(cos (5*x*y))**sin (10*(x*y-1))*(0.5-x*y)" -G "0.1*(0.5-x*y)*10*cos (2-x*y*y)*(sin (5*y*x))" -H 4 -m 3000 -r "(4096,4096)" -f 022.bmp```

## Example 4
![alt text](https://farm5.staticflickr.com/4652/25792678497_4514f19b96_k_d.jpg "Example")
```time stack exec -- two-danode-exe -F "(cos (5*x))*(1-x)*(x-1)*(0.5-x)" -G "20*cos (1-x)*(sin (5*y))*(1-y)" -H 5 -m 30 -r "(4096,4096)" -f 020.bmp```

## Example 5
![alt text](https://farm5.staticflickr.com/4664/39953335404_98a586d91f_k_d.jpg "Example")
```time stack exec -- two-danode-exe -F "(cos (5*x*y))*(1-x*y)*sin (10*(x*y-1))*(0.5-x*y)" -G "0.1*(0.5-x*y)*10*cos (1-x*y*y)*(sin (5*y*x))*(1-y*x)" -H 4 -m 3000 -r "(4096,4096)" -f 021.bmp```

## More Examples 
More examples can be found on my [Flickr account](https://flic.kr/s/aHskxk4mB5)
