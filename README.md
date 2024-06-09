# Magnetic-Structure
This code can help visualise the spin vectors of magnetic ions present in the unit cell.

It requires ELK's output files (GEOMETRY.OUT, INFO.OUT) and provides a GEOMETRY.xsf file, which can be opened via VESTA/XCrySDen or else utility software to visualise spin vectors or magnetic structure of the system.

**To compile:**

      gfortran magnetic_structure.f90 -o mag_struct

**To execute:**

      ./mag_struct
