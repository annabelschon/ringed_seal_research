The following is a list of Linux setup steps new SnowModel
users might be interested in.

# bash shell setup ideas.

Backup your original .bashrc file:

cp -p .bashrc .bashrc.orig

Edit your .bashrc file include any of the alias' and paths
listed in glens_dot_bashrc_file.txt.

In particular, you probably want to do this to make it easy
to run things from your current directory:

# put '.' at the head of my path
export PATH=".:$PATH"

I would also be sure and do this, it will give you room to
type commands on the command line:

# display the current path above the cursor line
export PS1='$PWD/\n yourname -> '

After making changes to your .bashrc file, it can be re-run
with:
> . .bashrc
  or
> source .bashrc


If you want to control the colors of different file names,
then copy the .dir_colors file to your home directory:

cp glens_dot_dir_colors_file.txt /home/yourname/.dir_colors


If you use vi for your editor, you might want to do these
things.

Copy the vimrc.txt file to your home directory:

cp glens_dot_vimrc_file.txt /home/yourname/.vimrc

If you want to use vi on the grads command line:

Copy the inputrc.txt file to your home directory:

cp glens_dot_inputrc_file.txt /home/yourname/.inputrc


