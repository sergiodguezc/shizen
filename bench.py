import statistics as stat

file = open("output.txt", "r+")

lines = [line.strip() for line in file.readlines()]

fmin = 0.0

values = []

times = []
n = len(lines) / 3

# Read data 
for i in range(len(lines)):
    # Just the title
    if i % 3 == 0:
        pass
    # Vector phase
    if i % 3 == 1:
        words = lines[i].split(',')
        value = words[-1][:-2]
        values.append(float(value))
    # Time
    if i % 3 == 2:
        # Drop the characters 'real '
        words = lines[i].split()
        times.append(float(words[1]))

def best_value():
    best = values[0]
    for val in values:
        if abs(val - fmin) < abs(best - fmin):
            best = val

    return best

bestv = best_value()
# print(f"best value: {bestv}")
print(bestv)

meanv = stat.mean(values)
# print(f"mean value: {meanv}")
print(meanv)

stdv = stat.stdev(values)
# print(f"std value: {stdv}")
print(stdv)

bestt = min(times)
# print(f"best time: {bestt}")
print(bestt)

meant = stat.mean(times)
# print(f"mean time: {meant}")
print(meant)

stdt = stat.stdev(times)
# print(f"std time: {stdt}")
print(stdt)
