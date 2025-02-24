This model and the associated calculations are documented in
Calum Cunningham's paper on collared moose temperatures in
Old Crow.


These calculations require SnowModel outputs of:

tair, tsfc, and wspd

or (in the .par file):

print_var_01 = y
print_var_03 = y

and:

print_var_26 = y

where you have put something like this in the outputs_user.f
code:

376             vars(i,j,26) = vars(i,j,26) + (Tsfc(i,j) - 273.15) /
377      &                     print_inc

