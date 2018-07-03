import sys,csv

def frange(start, stop, step):
    i = 0
    while start + i * step < stop:
        yield start + i * step
        i += 1

if __name__ == "__main__":
	try:
		fh = open(sys.argv[1],'a')
	except:
		print("Error: Failure opening file {}.".format(sys.argv[1]), file=sys.stderr)
		sys.exit(1)

	cur_writer = csv.writer(fh, delimiter=' ',
                            quotechar='|', quoting=csv.QUOTE_MINIMAL)
	for i in range(1000,51000,1000):
		for j in range(100,5100,100):
			for k in frange(0.01,0.06,0.01):
				cur_writer.writerow([i]+[j]+[k]+[100000,1])
