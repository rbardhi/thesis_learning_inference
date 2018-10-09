'''
Created on May 25, 2018

@author: nitesh
'''

# DATABASE_NAME = 'university'
#  
# ATTRIBUTES_DOMAIN_SETTING = dict(
#     course = dict(
#         diff = {'1': 'low', '2': 'high'},
#         rating = {'1': 'low', '2': 'high'}
#     ),
#     prof = dict(
#         teachingability = {'2': 'mid', '3': 'high'},
#         popularity = {'1': 'low', '2': 'high'}
#     ),
#     registration = dict(
#         grade = {'1': 'a', '2': 'b', '3': 'c', '4': 'd'}
#     )
# )
#  
# RANDOM_VARIABLE_PREDICATE = ['rating', 'diff', 'grade', 'sat', 'capability', 'salary', 'ranking', 'intelligence', 'teachingability', 'popularity']
# RANDOM_VARIABLE_PREDICATE_TYPE = dict(
#     diff = 'discrete',
#     sat = 'continuous',
#     capability = 'continuous',
#     salary = 'discrete',
#     ranking = 'continuous',
#     teachingability = 'discrete',
#     popularity = 'discrete',
#     rating = 'discrete',
#     grade = 'discrete',
#     intelligence = 'continuous'
# )
#  
# OMISSION_PERCENTAGE = 30
# OMISSION_PREDICATE = ['grade', 'rating']
# OMISSION_PREDICATE_DOMAIN = dict(
#     grade = ['a', 'b', 'c', 'd'],
#     rating = ['low', 'high']
# )
#  
# TEST_TRAIN_SPLIT = 20
# TARGET_PREDICATE = ['intelligence']


#####################  Hepatitis Database containing only discrete and boolean variables  #####################
# DATABASE_NAME = 'Hepatitis'
#  
# ATTRIBUTES_DOMAIN_SETTING = dict(
#     Bio = dict(
#         activity = {'0': 'zero', '1': 'one', '2': 'two', '3': 'three', 'FALSE': 'four'}
#     ),
#     indis = dict(
#         got = {'1': 'got0', '2': 'got1', '3': 'got2', '4': 'got3', '5': 'got4'},
#         gpt = {'1': 'gpt0', '2': 'gpt1', '3': 'gpt2', '4': 'gpt3'},
#         tcho = {'1': 'tcho0', '2': 'tcho1', '3': 'tcho2', '4': 'tcho3'},
#         tp = {'1': 'tp0', '2': 'tp1', '3': 'tp2', '4': 'tp3'},
#         ztt = {'1': 'a', '2': 'b', '3': 'c', '4': 'd', '5': 'e', '6': 'f'},
#         alb = {'1': 'low', '2': 'high'},
#         tbil = {'1': 'low', '2': 'high'},
#         dbil = {'1': 'low', '2': 'high'}
#     ),
#     inf = dict(
#         dur = {'0': 'dur0', '1': 'dur1', '2': 'dur2', '3': 'dur3', '4': 'dur4'}
#     )
# )
#  
# RANDOM_VARIABLE_PREDICATE = ['smokes', 'stress', 'rating','fibros', 'activity', 'sex', 'age', 'type', 'got', 'gpt', 'alb', 'tbil', 'dbil', 'che', 'ttt', 'ztt', 'tcho', 'tp', 'dur']
# RANDOM_VARIABLE_PREDICATE_TYPE = dict(
#     fibros = 'continuous',
#     activity = 'discrete',
#     sex = 'discrete',
#     age = 'continuous',
#     type = 'discrete',
#     got = 'discrete',
#     gpt = 'discrete',
#     alb = 'discrete',
#     tbil = 'discrete',
#     dbil = 'discrete',
#     che = 'continuous',
#     ttt = 'continuous',
#     ztt = 'discrete',
#     tcho = 'discrete',
#     tp = 'discrete',
#     dur = 'discrete'
# )
#  
# OMISSION_PERCENTAGE = 20
# OMISSION_PREDICATE = ['tcho', 'dur', 'activity', 'ztt']
# OMISSION_PREDICATE_DOMAIN = dict(
#     tcho = ['tcho0', 'tcho1', 'tcho2', 'tcho3'],
#     dur = ['dur0', 'dur1', 'dur2', 'dur3', 'dur4'],
#     activity = ['zero', 'one', 'two', 'three', 'four'],
#     ztt = ['a', 'b', 'c', 'd', 'e', 'f']
# )
#  
# TEST_TRAIN_SPLIT = 20
# TARGET_PREDICATE = ['type']





DATABASE_NAME = 'Hepatitis'
 
ATTRIBUTES_DOMAIN_SETTING = dict(
    Bio = dict(
        activity = {'0': 'zero', '1': 'one', '2': 'two', '3': 'three', 'FALSE': 'four'}
    ),
    indis = dict(
        alb = {'1': 'low', '2': 'high'},
        tbil = {'1': 'low', '2': 'high'},
        dbil = {'1': 'low', '2': 'high'},
        tcho = {'1': 'tcho0', '2': 'tcho1', '3': 'tcho2', '4': 'tcho3'}
    ),
    inf = dict(
        dur = {'0': 'dur0', '1': 'dur1', '2': 'dur2', '3': 'dur3', '4': 'dur4'}
    )
)
 
RANDOM_VARIABLE_PREDICATE = ['fibros', 'activity', 'sex', 'age', 'type', 'got', 'gpt', 'alb', 'tbil', 'dbil', 'che', 'ttt', 'ztt', 'tcho', 'tp', 'dur']
RANDOM_VARIABLE_PREDICATE_TYPE = dict(
    fibros = 'continuous',
    activity = 'discrete',
    sex = 'discrete',
    age = 'continuous',
    type = 'discrete',
    got = 'continuous',
    gpt = 'continuous',
    alb = 'discrete',
    tbil = 'discrete',
    dbil = 'discrete',
    che = 'continuous',
    ttt = 'continuous',
    ztt = 'continuous',
    tcho = 'discrete',
    tp = 'continuous',
    dur = 'discrete'
)
 
OMISSION_PERCENTAGE = 20
OMISSION_PREDICATE = ['tcho', 'dur', 'activity', 'ztt']
OMISSION_PREDICATE_DOMAIN = dict(
    tcho = ['tcho0', 'tcho1', 'tcho2', 'tcho3'],
    dur = ['dur0', 'dur1', 'dur2', 'dur3', 'dur4'],
    activity = ['zero', 'one', 'two', 'three', 'four'],
    ztt = []
)
 
TEST_TRAIN_SPLIT = 20
TARGET_PREDICATE = ['type']