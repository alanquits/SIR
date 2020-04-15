SIR Model for Spread of Disease
===============================

About
-----
This repo contains a barely tested implementation of the SIR Model 
that I wrote for practicing Haskell. A couple features that make my
implementation a little different from others are that
  * input is done with formatted ASCII files and writes to stdout
  * coefficients are transient

How to Use
----------
Hopefully, this repo should be simple to compile using stack.

An example of the input format is given in examples/sir_test.dat. 
The sections of the input file are:

  1. length of model run
  2. timestep size
  3. initial conditions for SIR variables (susceptible, infected, removed)
  4. beta coefficient initial condition (and possible transient values)
  5. gamma coefficient initial condition (and possible transient values)

In the example, beta is constant throughout the model and gamma is transient.

License
-------
MIT. If any of this is useful for you, do whatever you want with it.


