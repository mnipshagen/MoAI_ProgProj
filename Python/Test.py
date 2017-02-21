import numpy as np
import json

query = json.load(open('query1.json'))

columns = ['teachers','lectures','rooms','timeslots']

for fields in query:
	data = tuple(fields[c] for c in columns)

teachers = data[0]
lectures = data[1]
rooms = data[2]
timeslots = data[3]

	