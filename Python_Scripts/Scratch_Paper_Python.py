# Scratch Paper used for learning purposes

# To run this code, open the Command Prompt/Shell and type the following:
# python.exe "C:\Users\Wesley\wessport\github\Scratch_Paper_Python.py"


ingredients = ['snails', 'leeches', 'gorilla belly-button lint',
               'caterpillar eyebrows', 'centipede toes']

Earth_weight = 130
Moon_weight = Earth_weight * 0.165
year = 1
j = 0

for i in range(0, 14):
    Moon_weight = Moon_weight + j
    print(('%s %s') % (year, Moon_weight))
    year = year + 1
    j = Moon_weight
