## this code submit R job with arguments in for loop using SLURM on longleaf.unc.edu

## submit run_R_code.R with different input parameters and runs in different jobs
## this code can be run under R interactive mode in linux shell

dataset = 'lasso'
for(i in 1:10){
	wrap_cmd = sprintf("srun R CMD BATCH --slave '--args %s %s' run_R_code.R", dataset, i)
	cmd1 = sprintf("sbatch --ntasks=1 --time=1:00 --mem=10 --wrap=\"%s\"",wrap_cmd)
	print(cmd1)
	system(cmd1)
}

