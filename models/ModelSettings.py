'''
Created on Jun 2, 2018

@author: nitesh
'''

MLR_SETTINGS = dict(
    weight_precision = 0.01,
    bias_precision = 1.0,
    optimization_method = 'BFGS',
    tolerance = 1e-06,
    maximum_iteration = 100,
    message_display = True
)

LR_SETTINGS = dict(
    weight_precision = 0.000001,
    optimization_method = 'BFGS',
    tolerance = 1e-06,
    maximum_iteration = 100,
    message_display = True
)

