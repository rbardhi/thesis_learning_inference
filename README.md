Installation
============

1) Install Yap Prolog following steps here: https://github.com/davidenitti/DC

2) Install python packages: Numpy, Sklearn, Cython

3) Build PyDC executable file
	
	$ cd yapInterface
   	$ python setup.py build_ext --inplace
   	$ mv yapWrapper.so ../core/
   	
Execution 
=========

$ python TreeLearnerProbabilistic.py