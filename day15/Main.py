ins = [1,20,8,12,0,14]
lookedAt = {}
stepsTaken = 1
maxSteps = 30000000
for i in ins[:-1]:
    lookedAt[i] = stepsTaken
    stepsTaken += 1

lastSeen = ins[-1]

while stepsTaken < maxSteps:
    if lastSeen in lookedAt:
        prev = lookedAt[lastSeen]
        lookedAt[lastSeen] = stepsTaken
        lastSeen = stepsTaken - prev
    else:
        lookedAt[lastSeen] = stepsTaken
        lastSeen = 0
    stepsTaken += 1

        
#print(lookedAt)
print(lastSeen)