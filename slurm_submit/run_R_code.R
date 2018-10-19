## command line in interactive mode
# srun R CMD BATCH --slave "--args lasso 1" run_R_code.R


### use another R code or shell script to submit R job in batch
## command line in batch mode to submit multiple jobs
# sbatch --ntasks=1 --time=1:00 --mem=10 --wrap="srun R CMD BATCH --slave '--args lasso 1' run_R_code.R"

args = commandArgs(TRUE)

dataset = args[1]
i = as.numeric(args[2])

set.seed(i)

# i = 1; dataset='lasso';
mx1 = matrix(rnorm(20*10),20,10,byrow=TRUE)
write.csv(mx1,file=sprintf('mx_data_%s_%s.csv',dataset,i),row.names=FALSE,quote=FALSE)

