#!/bin/bash

# Simple limit cycle
time stack exec -- two-danode-exe -P -R "r*(1-r^2)" -T "1" -H 1.5 -c "(0,0)" -m 20 -r "(4096,4096)" -f 001.bmp

# Spiral with homoclinic orbit
time stack exec -- two-danode-exe -F "y-y^3" -G "-x-y^2" -H 2 -c "(0,0)" -m 20 -r "(4096,4096)" -f 002.bmp

time stack exec -- two-danode-exe -F "-2*cos x - cos y" -G "-2*cos y - cos x" -H 4 -c "(0,0)" -m 5 -r "(4096,4096)" -f 003.bmp

# Pendulum
time stack exec -- two-danode-exe -F "y" -G "-sin x" -H 8 -c "(0,0.0)" -m 10 -r "(4096,4096)" -f 004.bmp

# Damped pendulum
time stack exec -- two-danode-exe -F "y" -G "-1*y-sin x" -H 8 -c "(0,0.0)" -m 10 -r "(4096,4096)" -f 005.bmp

# Van der pol oscillator
time stack exec -- two-danode-exe -F "y" -G "-x+y*(1-x^2)" -H 4 -c "(0,0)" -m 10 -r "(4096,4096)" -f 006.bmp

time stack exec -- two-danode-exe -F "y^3 - 4*x" -G "y^3 - y - 3*x" -H 6 -c "(0,0)" -m 1 -r "(4096,4096)" -f 007.bmp
time stack exec -- two-danode-exe -F "y^3 - 12*x" -G "y^3 - y - 10*x" -H 3 -c "(0,0)" -m 10 -r "(4096,4096)" -f 008.bmp
time stack exec -- two-danode-exe -F "-x+1*y+x*x*y" -G "1-1*y-x*x*y" -H 2 -c "(0,0.5)" -m 100 -r "(4096,4096)" -f 009.bmp

# Infinite-period bifurcations
time stack exec -- two-danode-exe -R "r*(1-r*r)" -T "1 - sin theta" -P -H 2 -m 10 -r "(4096,4096)" -f 010.bmp
time stack exec -- two-danode-exe -R "r*(1-r*r)" -T "0.5 - sin theta" -P -H 2 -m 20 -r "(4096,4096)" -f 011.bmp

time stack exec -- two-danode-exe -F "1 - (1+1)*x+1*x*x*y" -G "1*x-1*x*x*y" -H 5 -m 20 -r "(4096,4096)" -f 012.bmp

# Competition models for shared resource
time stack exec -- two-danode-exe -F "x*(1-x/100) - x*y" -G "y*(1-y/20)-x*y" -H 2 -m 10 -r "(4096,4096)" -f 013.bmp
time stack exec -- two-danode-exe -F "x*(1-x/10) - x*y" -G "y*(1-y/5)-x*y" -H 5 -r "(4096,4096)" -f 014.bmp
time stack exec -- two-danode-exe -F "1.25*x*(1-x/10) - x*y" -G "y*(1-y/5)-x*y" -H 5 -m 10 -r "(4096,4096)" -f 015.bmp

# Predator prey model
time stack exec -- two-danode-exe -F "0.75*x*(1-x/2) - 2*x*y/(1+x)" -G "-y + 2*x*y/(1+x)" -H 0.75 -c "(0.75,0.75)" -m 20 -r "(4096,4096)" -f 016.bmp

time stack exec -- two-danode-exe -P -R "2*(1-2*sin theta + r * (1-r*r*r))" -T "1 - 0.5*cos theta + sin theta * r" -H 1.5 -c "(0,-0.25)" -m 8 -r "(4096,4096)" -f 017.bmp
time stack exec -- two-danode-exe -P -c "(0,0)" -R "r*(1-r)*(r-1)*(0.5-r)" -T "sin (12*theta)" -H 2.2  -m 300 -r "(4096,4096)" -f 018.bmp
time stack exec -- two-danode-exe -P -c "(0,0)" -m 300 -R "r*(r-1)*(0.5-r)" -T "sin (12*theta)*(cos (8*theta))" -H 2 -r "(4096,4096)" -f 019.bmp
time stack exec -- two-danode-exe -F "(cos (5*x))*(1-x)*(x-1)*(0.5-x)" -G "20*cos (1-x)*(sin (5*y))*(1-y)" -H 5 -m 30 -r "(4096,4096)" -f 020.bmp
time stack exec -- two-danode-exe -F "(cos (5*x*y))*(1-x*y)*sin (10*(x*y-1))*(0.5-x*y)" -G "0.1*(0.5-x*y)*10*cos (1-x*y*y)*(sin (5*y*x))*(1-y*x)" -H 4 -m 3000 -r "(4096,4096)" -f 021.bmp
time stack exec -- two-danode-exe -F "(cos (5*x*y))**sin (10*(x*y-1))*(0.5-x*y)" -G "0.1*(0.5-x*y)*10*cos (2-x*y*y)*(sin (5*y*x))" -H 4 -m 3000 -r "(4096,4096)" -f 022.bmp
time stack exec -- two-danode-exe -F "5*(cos (5*x*y))**sin (10*(x*y-1))*(0.5-x*y)" -G "0.1*(0.5-x*y)*10*cos (2-x*y*y)*(sin (5*y*x))" -H 4 -m 3000 -r "(4096,4096)" -f 023.bmp
time stack exec -- two-danode-exe -F "2*exp(cos (10*x) - sin (30*y))*sin(0.5 - 10*x*y)" -G "2*exp(sin (20*x) - cos(30*y))*sin(1 + 10*x*y)" -H 1 -m 3 -r "(4096,4096)" -f 024.bmp
time stack exec -- two-danode-exe -F "2*exp(cos (10*x*y) - sin (30*y*x))*sin(0.5 - 10*x*y)" -G "2*exp(sin (20*x) - cos(30*x*y))*sin(1 + 10*x*y)" -H 1 -m 3 -r "(4096,4096)" -f 025.bmp
time stack exec -- two-danode-exe -F "2*exp(cos (10*x*y) - sin (30*y*x))*sin(0.5 - 10*x*y)" -G "2*exp(sin (20*sqrt (abs (x*x*y))) - cos(30*x*y))*sin(1 + 10*x*y)" -H 1.3 -m 3 -r "(4096,4096)" -f 026.bmp
time stack exec -- two-danode-exe -F "x" -G "y" -H 8 -c "(0,0.0)" -m 10 -r "(4096,4096)" -f 027.bmp
time stack exec -- two-danode-exe -F "-2*cos x - cos y" -G "-2*cos y - cos x" -H 4 -c "(0,0)" -m 0.1 -r "(4096,4096)" -f 028.bmp
time stack exec -- two-danode-exe -F "y-y^3" -G "-x-y^2" -H 2 -c "(0,0)" -m 3 -r "(4096,4096)" -f 029.bmp
time stack exec -- two-danode-exe -F "-2*cos x - cos y" -G "-2*cos y - cos x" -H 4 -c "(0,0)" -r "(4096,4096)" -f 030.bmp
time stack exec -- two-danode-exe -F "2*exp(cos (10*x*y) - sin (30*y*x))*sin(0.5 - 10*x*y)" -G "2*exp(sin (20*sqrt (abs (x*x*y))) - cos(30*x*y))*sin(1 + 10*x*y)" -H 1.5 -r "(4096,4096)" -f 031.bmp
time stack exec -- two-danode-exe -H 5 -F "sin x - cos (2.3*y)" -G "cos y - sin (4*x)" -r "(4096,4096)" -f 032.bmp
time stack exec -- two-danode-exe -P -R "2*(1-2*sin theta + r * (1-r*r*r))" -T "1 - 0.5*cos theta + sin theta * r" -H 1.5 -c "(0,-0.25)" -r "(4096,4096)" -f 033.bmp
time stack exec -- two-danode-exe -P -c "(0,0)" -R "r*(1-r)*(r-1)*(0.5-r)" -T "sin (12*theta)" -H 2.2 -r "(4096,4096)" -f 034.bmp
time stack exec -- two-danode-exe -P -c "(0,0)" -R "r*(r-1)*(0.5-r)" -T "sin (12*theta)*(cos (8*theta))" -H 2 -r "(4096,4096)" -f 035.bmp
time stack exec -- two-danode-exe -F "(cos (5*x))*(1-x)*(x-1)*(0.5-x)" -G "20*cos (1-x)*(sin (5*y))*(1-y)" -H 5 -r "(4096,4096)" -f 036.bmp
time stack exec -- two-danode-exe -F "(cos (5*x*y))*(1-x*y)*sin (10*(x*y-1))*(0.5-x*y)" -G "0.1*(0.5-x*y)*10*cos (1-x*y*y)*(sin (5*y*x))*(1-y*x)" -H 4 -r "(4096,4096)" -f 037.bmp
time stack exec -- two-danode-exe -F "(cos (5*x*y))**sin (10*(x*y-1))*(0.5-x*y)" -G "0.1*(0.5-x*y)*10*cos (2-x*y*y)*(sin (5*y*x))" -H 4 -r "(4096,4096)" -f 038.bmp
time stack exec -- two-danode-exe -F "5*(cos (5*x*y))**sin (10*(x*y-1))*(0.5-x*y)" -G "0.1*(0.5-x*y)*10*cos (2-x*y*y)*(sin (5*y*x))" -H 4 -r "(4096,4096)" -f 039.bmp
time stack exec -- two-danode-exe -F "2*exp(cos (10*x) - sin (30*y))*sin(0.5 - 10*x*y)" -G "2*exp(sin (20*x) - cos(30*y))*sin(1 + 10*x*y)" -H 1 -r "(4096,4096)" -f 040.bmp
time stack exec -- two-danode-exe -F "2*exp(cos (10*x*y) - sin (30*y*x))*sin(0.5 - 10*x*y)" -G "2*exp(sin (20*x) - cos(30*x*y))*sin(1 + 10*x*y)" -H 1 -r "(4096,4096)" -f 041.bmp
time stack exec -- two-danode-exe -F "2*exp(cos (10*x*y) - sin (30*y*x))*sin(0.5 - 10*x*y)" -G "2*exp(sin (20*sqrt (abs (x*x*y))) - cos(30*x*y))*sin(1 + 10*x*y)" -H 1.3 -r "(4096,4096)" -f 042.bmp
time stack exec -- two-danode-exe -H 5 -F "(abs $ cos (0.75*x))**(abs $ cos (3*y)) + sin (3*x)" -G "sin x - cos y" -r "(4096,4096)" -f 043.bmp
time stack exec -- two-danode-exe -H 5 -F "(1.5*sin (3*x*y -10))*(abs $ cos (1.75*x))**(abs $ 3 * cos (3*y)) + sin (3*x)" -G "sin (4.3*x) - cos (0.5*y)" -r "(4096,4096)" -f 044.bmp
time stack exec -- two-danode-exe -H 8 -F "(1.5*sin (3*x*y -10))*(abs $ cos (1.75*x))**(abs $ 3 * cos (3*y)) + sin (3*x)" -G "sin (4.3*x) - cos (0.5*y)" -r "(4096,4096)" -f 045.bmp
time stack exec -- two-danode-exe -H 5 -r "(4096,4096)" -f 046.bmp -F "sin(2*y - 3*x)*exp(-abs (sin (3*x+1-2*y) - cos(x-3*y+1)))" -G "exp(-abs (3*cos y - 2*sin (1*x) + 3*cos(x*y) - 2.5*sin(0.5*x*y)))"
time stack exec -- two-danode-exe -H 1 -r "(4096,4096)" -f 047.bmp -F "sin(2*y - 3*x)*exp(-abs (sin (3*x+1-2*y) - cos(x-3*y+1)))" -G "cos(2*y - 3*x)*exp(-abs (cos (3*x+1-2*y) - sin(x-3*y+1)))"
time stack exec -- two-danode-exe -H 3 -r "(4096,4096)" -f 048.bmp -F "sin(2*y - 3*x)*exp(-abs (sin (3*x+1-2*y) - cos(x-3*y+1)))" -G "cos(2*y - 3*x)*exp(-abs (cos (3*x+1-2*y) - sin(x-3*y+1)))"
time stack exec -- two-danode-exe -H 5 -r "(4096,4096)" -f 049.bmp -F "sin(2*y - 3*x)*exp(-abs (sin (3*x+1-2*y) - cos(x-3*y+1)))" -G "cos(2*y - 3*x)*exp(-abs (cos (3*x+1-2*y) - sin(x-3*y+1)))"
time stack exec -- two-danode-exe -H 3 -r "(4096,4096)" -f 050.bmp -F "sin(2*y - 3*x)*exp(-abs (sin (3*x+1-2*y) - cos(x-3*y+1)))" -G "cos(2*y - 3*x)*exp(-abs (cos (3*x+1-2*y) - cos(x-3*y+1)))"
time stack exec -- two-danode-exe -H 5 -r "(4096,4096)" -f 051.bmp -F "sin(2*y - 3*x)*exp(-abs (sin (3*x+1-2*y) - cos(x-3*y+1)))" -G "cos(2*y - 3*x)*exp(-abs (cos (3*x+1-2*y) - cos(x-3*y+1)))"
time stack exec -- two-danode-exe -H 5 -r "(4096,4096)" -f 052.bmp -F "cos(2*y - 3*x)*exp(-abs (cos (3*x+1-2*y) - cos(x-3*y+1)))" -G "cos(2*y - 3*x)*exp(-abs (cos (3*x+1-2*y) - cos(x-3*y+1)))"
