If you want to run this in the background, you have two
options (same for the zip_files_in_directory.script file):

1) This runs it as a batch (or 'at') job.

> at now
> zip_ascii_files.script
> ctrl^d

2) Note that if you do it this way, if you kill the terminal
window, then the job also dies.

> zip_ascii_files.script &

