#!/bin/bash

res=${1:-"(4096,4096)"}

# Simple limit cycle
time stack exec -- two-danode-exe --write-json -P -R "r*(1-r^2)" -T "1" -H 1.5 -c "(0,0)" -m 20 -r "$res" -f ${2}001.png

# Spiral with homoclinic orbit
time stack exec -- two-danode-exe --write-json -F "y-y^3" -G "-x-y^2" -H 2 -c "(0,0)" -m 20 -r "$res" -f ${2}002.png

time stack exec -- two-danode-exe --write-json -F "-2*cos x - cos y" -G "-2*cos y - cos x" -H 4 -c "(0,0)" -m 5 -r "$res" -f ${2}003.png

# Pendulum
time stack exec -- two-danode-exe --write-json -F "y" -G "-sin x" -H 8 -c "(0,0.0)" -m 10 -r "$res" -f ${2}004.png

# Damped pendulum
time stack exec -- two-danode-exe --write-json -F "y" -G "-1*y-sin x" -H 8 -c "(0,0.0)" -m 10 -r "$res" -f ${2}005.png

# Van der pol oscillator
time stack exec -- two-danode-exe --write-json -F "y" -G "-x+y*(1-x^2)" -H 4 -c "(0,0)" -m 10 -r "$res" -f ${2}006.png

time stack exec -- two-danode-exe --write-json -F "y^3 - 4*x" -G "y^3 - y - 3*x" -H 6 -c "(0,0)" -m 1 -r "$res" -f ${2}007.png
time stack exec -- two-danode-exe --write-json -F "y^3 - 12*x" -G "y^3 - y - 10*x" -H 3 -c "(0,0)" -m 10 -r "$res" -f ${2}008.png
time stack exec -- two-danode-exe --write-json -F "-x+1*y+x*x*y" -G "1-1*y-x*x*y" -H 2 -c "(0,0.5)" -m 100 -r "$res" -f ${2}009.png

# Infinite-period bifurcations
time stack exec -- two-danode-exe --write-json -R "r*(1-r*r)" -T "1 - sin theta" -P -H 2 -m 10 -r "$res" -f ${2}010.png
time stack exec -- two-danode-exe --write-json -R "r*(1-r*r)" -T "0.5 - sin theta" -P -H 2 -m 20 -r "$res" -f ${2}011.png

time stack exec -- two-danode-exe --write-json -F "1 - (1+1)*x+1*x*x*y" -G "1*x-1*x*x*y" -H 5 -m 20 -r "$res" -f ${2}012.png

# Competition models for shared resource
time stack exec -- two-danode-exe --write-json -F "x*(1-x/100) - x*y" -G "y*(1-y/20)-x*y" -H 2 -m 10 -r "$res" -f ${2}013.png
time stack exec -- two-danode-exe --write-json -F "x*(1-x/10) - x*y" -G "y*(1-y/5)-x*y" -H 5 -r "$res" -f ${2}014.png
time stack exec -- two-danode-exe --write-json -F "1.25*x*(1-x/10) - x*y" -G "y*(1-y/5)-x*y" -H 5 -m 10 -r "$res" -f ${2}015.png

# Predator prey model
time stack exec -- two-danode-exe --write-json -F "0.75*x*(1-x/2) - 2*x*y/(1+x)" -G "-y + 2*x*y/(1+x)" -H 0.75 -c "(0.75,0.75)" -m 20 -r "$res" -f ${2}016.png

time stack exec -- two-danode-exe --write-json -P -R "2*(1-2*sin theta + r * (1-r*r*r))" -T "1 - 0.5*cos theta + sin theta * r" -H 1.5 -c "(0,-0.25)" -m 8 -r "$res" -f ${2}017.png
time stack exec -- two-danode-exe --write-json -P -c "(0,0)" -R "r*(1-r)*(r-1)*(0.5-r)" -T "sin (12*theta)" -H 2.2  -m 300 -r "$res" -f ${2}018.png
time stack exec -- two-danode-exe --write-json -P -c "(0,0)" -m 300 -R "r*(r-1)*(0.5-r)" -T "sin (12*theta)*(cos (8*theta))" -H 2 -r "$res" -f ${2}019.png
time stack exec -- two-danode-exe --write-json -F "(cos (5*x))*(1-x)*(x-1)*(0.5-x)" -G "20*cos (1-x)*(sin (5*y))*(1-y)" -H 5 -m 30 -r "$res" -f ${2}020.png
time stack exec -- two-danode-exe --write-json -F "(cos (5*x*y))*(1-x*y)*sin (10*(x*y-1))*(0.5-x*y)" -G "0.1*(0.5-x*y)*10*cos (1-x*y*y)*(sin (5*y*x))*(1-y*x)" -H 4 -m 3000 -r "$res" -f ${2}021.png
time stack exec -- two-danode-exe --write-json -F "(cos (5*x*y))**sin (10*(x*y-1))*(0.5-x*y)" -G "0.1*(0.5-x*y)*10*cos (2-x*y*y)*(sin (5*y*x))" -H 4 -m 3000 -r "$res" -f ${2}022.png
time stack exec -- two-danode-exe --write-json -F "5*(cos (5*x*y))**sin (10*(x*y-1))*(0.5-x*y)" -G "0.1*(0.5-x*y)*10*cos (2-x*y*y)*(sin (5*y*x))" -H 4 -m 3000 -r "$res" -f ${2}023.png
time stack exec -- two-danode-exe --write-json -F "2*exp(cos (10*x) - sin (30*y))*sin(0.5 - 10*x*y)" -G "2*exp(sin (20*x) - cos(30*y))*sin(1 + 10*x*y)" -H 1 -m 3 -r "$res" -f ${2}024.png
time stack exec -- two-danode-exe --write-json -F "2*exp(cos (10*x*y) - sin (30*y*x))*sin(0.5 - 10*x*y)" -G "2*exp(sin (20*x) - cos(30*x*y))*sin(1 + 10*x*y)" -H 1 -m 3 -r "$res" -f ${2}025.png
time stack exec -- two-danode-exe --write-json -F "2*exp(cos (10*x*y) - sin (30*y*x))*sin(0.5 - 10*x*y)" -G "2*exp(sin (20*sqrt (abs (x*x*y))) - cos(30*x*y))*sin(1 + 10*x*y)" -H 1.3 -m 3 -r "$res" -f ${2}026.png
time stack exec -- two-danode-exe --write-json -F "x" -G "y" -H 8 -c "(0,0.0)" -m 10 -r "$res" -f ${2}027.png
time stack exec -- two-danode-exe --write-json -F "-2*cos x - cos y" -G "-2*cos y - cos x" -H 4 -c "(0,0)" -m 0.1 -r "$res" -f ${2}028.png
time stack exec -- two-danode-exe --write-json -F "y-y^3" -G "-x-y^2" -H 2 -c "(0,0)" -m 3 -r "$res" -f ${2}029.png
time stack exec -- two-danode-exe --write-json -F "-2*cos x - cos y" -G "-2*cos y - cos x" -H 4 -c "(0,0)" -r "$res" -f ${2}030.png
time stack exec -- two-danode-exe --write-json -F "2*exp(cos (10*x*y) - sin (30*y*x))*sin(0.5 - 10*x*y)" -G "2*exp(sin (20*sqrt (abs (x*x*y))) - cos(30*x*y))*sin(1 + 10*x*y)" -H 1.5 -r "$res" -f ${2}031.png
time stack exec -- two-danode-exe --write-json -H 5 -F "sin x - cos (2.3*y)" -G "cos y - sin (4*x)" -r "$res" -f ${2}032.png
time stack exec -- two-danode-exe --write-json -P -R "2*(1-2*sin theta + r * (1-r*r*r))" -T "1 - 0.5*cos theta + sin theta * r" -H 1.5 -c "(0,-0.25)" -r "$res" -f ${2}033.png
time stack exec -- two-danode-exe --write-json -P -c "(0,0)" -R "r*(1-r)*(r-1)*(0.5-r)" -T "sin (12*theta)" -H 2.2 -r "$res" -f ${2}034.png
time stack exec -- two-danode-exe --write-json -P -c "(0,0)" -R "r*(r-1)*(0.5-r)" -T "sin (12*theta)*(cos (8*theta))" -H 2 -r "$res" -f ${2}035.png
time stack exec -- two-danode-exe --write-json -F "(cos (5*x))*(1-x)*(x-1)*(0.5-x)" -G "20*cos (1-x)*(sin (5*y))*(1-y)" -H 5 -r "$res" -f ${2}036.png
time stack exec -- two-danode-exe --write-json -F "(cos (5*x*y))*(1-x*y)*sin (10*(x*y-1))*(0.5-x*y)" -G "0.1*(0.5-x*y)*10*cos (1-x*y*y)*(sin (5*y*x))*(1-y*x)" -H 4 -r "$res" -f ${2}037.png
time stack exec -- two-danode-exe --write-json -F "(cos (5*x*y))**sin (10*(x*y-1))*(0.5-x*y)" -G "0.1*(0.5-x*y)*10*cos (2-x*y*y)*(sin (5*y*x))" -H 4 -r "$res" -f ${2}038.png
time stack exec -- two-danode-exe --write-json -F "5*(cos (5*x*y))**sin (10*(x*y-1))*(0.5-x*y)" -G "0.1*(0.5-x*y)*10*cos (2-x*y*y)*(sin (5*y*x))" -H 4 -r "$res" -f ${2}039.png
time stack exec -- two-danode-exe --write-json -F "2*exp(cos (10*x) - sin (30*y))*sin(0.5 - 10*x*y)" -G "2*exp(sin (20*x) - cos(30*y))*sin(1 + 10*x*y)" -H 1 -r "$res" -f ${2}040.png
time stack exec -- two-danode-exe --write-json -F "2*exp(cos (10*x*y) - sin (30*y*x))*sin(0.5 - 10*x*y)" -G "2*exp(sin (20*x) - cos(30*x*y))*sin(1 + 10*x*y)" -H 1 -r "$res" -f ${2}041.png
time stack exec -- two-danode-exe --write-json -F "2*exp(cos (10*x*y) - sin (30*y*x))*sin(0.5 - 10*x*y)" -G "2*exp(sin (20*sqrt (abs (x*x*y))) - cos(30*x*y))*sin(1 + 10*x*y)" -H 1.3 -r "$res" -f ${2}042.png
time stack exec -- two-danode-exe --write-json -H 5 -F "(abs $ cos (0.75*x))**(abs $ cos (3*y)) + sin (3*x)" -G "sin x - cos y" -r "$res" -f ${2}043.png
time stack exec -- two-danode-exe --write-json -H 5 -F "(1.5*sin (3*x*y -10))*(abs $ cos (1.75*x))**(abs $ 3 * cos (3*y)) + sin (3*x)" -G "sin (4.3*x) - cos (0.5*y)" -r "$res" -f ${2}044.png
time stack exec -- two-danode-exe --write-json -H 8 -F "(1.5*sin (3*x*y -10))*(abs $ cos (1.75*x))**(abs $ 3 * cos (3*y)) + sin (3*x)" -G "sin (4.3*x) - cos (0.5*y)" -r "$res" -f ${2}045.png
time stack exec -- two-danode-exe --write-json -H 5 -r "$res" -f ${2}046.png -F "sin(2*y - 3*x)*exp(-abs (sin (3*x+1-2*y) - cos(x-3*y+1)))" -G "exp(-abs (3*cos y - 2*sin (1*x) + 3*cos(x*y) - 2.5*sin(0.5*x*y)))"
time stack exec -- two-danode-exe --write-json -H 1 -r "$res" -f ${2}047.png -F "sin(2*y - 3*x)*exp(-abs (sin (3*x+1-2*y) - cos(x-3*y+1)))" -G "cos(2*y - 3*x)*exp(-abs (cos (3*x+1-2*y) - sin(x-3*y+1)))"
time stack exec -- two-danode-exe --write-json -H 3 -r "$res" -f ${2}048.png -F "sin(2*y - 3*x)*exp(-abs (sin (3*x+1-2*y) - cos(x-3*y+1)))" -G "cos(2*y - 3*x)*exp(-abs (cos (3*x+1-2*y) - sin(x-3*y+1)))"
time stack exec -- two-danode-exe --write-json -H 5 -r "$res" -f ${2}049.png -F "sin(2*y - 3*x)*exp(-abs (sin (3*x+1-2*y) - cos(x-3*y+1)))" -G "cos(2*y - 3*x)*exp(-abs (cos (3*x+1-2*y) - sin(x-3*y+1)))"
time stack exec -- two-danode-exe --write-json -H 3 -r "$res" -f ${2}050.png -F "sin(2*y - 3*x)*exp(-abs (sin (3*x+1-2*y) - cos(x-3*y+1)))" -G "cos(2*y - 3*x)*exp(-abs (cos (3*x+1-2*y) - cos(x-3*y+1)))"
time stack exec -- two-danode-exe --write-json -H 5 -r "$res" -f ${2}051.png -F "sin(2*y - 3*x)*exp(-abs (sin (3*x+1-2*y) - cos(x-3*y+1)))" -G "cos(2*y - 3*x)*exp(-abs (cos (3*x+1-2*y) - cos(x-3*y+1)))"
time stack exec -- two-danode-exe --write-json -H 5 -r "$res" -f ${2}052.png -F "cos(2*y - 3*x)*exp(-abs (cos (3*x+1-2*y) - cos(x-3*y+1)))" -G "cos(2*y - 3*x)*exp(-abs (cos (3*x+1-2*y) - cos(x-3*y+1)))"

#More Competition models for shared resources
time stack exec -- two-danode-exe --write-json -F "x*(3-x - 2*y)" -G "y*(2-x-y)" -H 4 -m 10 -r "$res" -f ${2}053.png
time stack exec -- two-danode-exe --write-json -F "x*(1-x/7) - x*y" -G "y*(1-y/4)-x*y" -H 10 -m 10 -r "$res" -f ${2}054.png

time stack exec -- two-danode-exe --write-json -F "sin(13*x)*cos(10*y*x)" -G "sin(5*x)*cos (10*y*x)" -H 2 -r "$res" -f ${2}055.png

time stack exec -- two-danode-exe --write-json -R "r" -T "cos (r*20)*sin (theta*20)" -H 1 -r "$res" -f ${2}056.png
