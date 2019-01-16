# predict_replication.R 
This script predicts the replication rate of a replication study.

Usage:

Rscript predict_replication.R <SUMMARY_STATS> <OUT_PREFIX>

This script has two required arguments:
-  <SUMMARY_STATS> A tab deliminated file that has the columns S1, S2, N1, N2 corresponding to the two summary statistics and two sample sizes. Each line after the header should have the information for one significant variant.  
-  <OUT_PREFIX> An output prefix for the text and figure outputs

Output:
-  <OUT_PREFIX>.png A figure showing the expected summary statistics of the replication study given the initial study, along with 95% confidence intervals.
-  <OUT_PREFIX>.txt A text file containing the following columns
	1. <SUMMARY_STATS>
	2. True replication rate
	3. True replication count
	4. Predicted replication rate (WC)
	5. Sigma_g^2 (WC)
	6. Slope (WC)
	7. Sd (WC)
	8. Predicted replication count (WC)
	9. Predicted replication rate (WC + C)
	10. Sigma_g^2 (WC + C)
	11. Sigma_c1^2 (WC + C)
	12. Sigma_C2^2 (WC + C)
	13. Slope (WC+C)
	14. Sd (WC + C)
	15. Predicted replication count (WC+C)

