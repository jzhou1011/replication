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
	N_GWAS_max = int(sys.argv[2])
	N_Rep_max = int(sys.argv[3])
	lambda_max = float(sys.argv[4])
	for i in range(1000,N_GWAS_max+1000,1000):
		for j in range(100,N_Rep_max+100,100):
			for k in frange(0.01,lambda_max+0.01,0.01):
				cur_writer.writerow([i]+[j]+[k]+[100000,1])
