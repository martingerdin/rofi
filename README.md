# Getting started

`rofi` is a R package to help structure and streamline "Opportunities
for improvement" (OFI) research projects using data from the Karolinska
University Hospital trauma registry and trauma care quality
database. 

This document will get you started with your OFI project and is more
than an introduction to the R package. So, this document will guide
you through the following steps:

1. Setting up a GitHub project
2. Signing in to our RStudio test server
3. Importing your GitHub project into the RStudio environment
4. Creating your OFI project using `rofi`
5. Pushing your edits from the RStudio environment back to GitHub

## Setting up a GitHub project

Before you do anything else, please sign up for a [GitHub
account](https://github.com). Create a new, empty, public repository
by following [GitHub's own
guide](https://docs.github.com/en/get-started/quickstart/create-a-repo).
Please use a short project name, for example if your project is about
how early interventions are associated with opportunities for
improvement, use `early-interventions-ofi` as the repository name. **Do
not initialize your repository with a README**.

## Signing in to our RStudio test server

Once you have created your GitHub repository, browse to our [RStudio
test server](https://rstudio.test.noacs.io) and sign in with the
credentials sent to you over email or Slack.

## Importing your GitHub project into the RStudio environment

Once logged in to RStudio server you will need to setup a way to
access GitHub. We recommend using a so called SSH key to do
this. Click Tools > Global Options... > Git/SVN > Create RSA key. You
can leave the Passphrase blank. Click Create > Close. Click View
public key and copy the key to the clipboard. [Then follow GitHub's
guide on how to add a SSH key to your
account](https://docs.github.com/en/authentication/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account).

Once you have added the SSH key to your GitHub account click File >
New Project > Version Control > Git. You will now need to enter the
SSH version of your repository url, which you find if you browse to
your GitHub repository, click the green button Code and select
SSH. Copy the url that starts with git@github.com and paste it into
Repository url. Then enter the directory name, typically the same as
the repository name. Finally, click Create Project.

## Creating your OFI project using `rofi`

Now, in the R console, type (assuming the name of your project is `early-interventions-ofi`):

```{r, eval = FALSE}
devtools::install_github("martingerdin/rofi", build_vignettes = TRUE, build_manual = TRUE)
library(rofi)
create("early-interventions-ofi")
```

When prompted, enter the username and password for database access
that you have been sent on email or Slack.

You should now have multiple new files and directories in your project
directory, and RStudio should also have opened the file manuscript.Rmd
for you. Start by reading through that file and once you're done, open
the file main.R. For now, add the line `library("rofi")` just above
the line that says `noacsr::source_all_functions()` and make sure you
save. Then go back to your manuscript.Rmd file and click knit, and a
new window with the compiled file should pop up.

## Pushing your edits from the RStudio environment back to GitHub

Now when you have created all these new files and folders you want to
replicate your edits on the server onto GitHub. To do that, click Git
in the project panel, its the panel in the upper right corner. Click
the tick box under Staged in front of all files and then click
Commit. Enter a short description when prompted, for example "Add
files and folders" and then click Commit. Hopefully it will succeed
and you can then click Close. Click Push to "push" your edits to
GitHub. Browse to your GitHub repository to verify that your files and
folders also appear there.

## Some tips for a smoother development experience

- Do as much work as possible in RStudio, rather than on GitHub. 
- Commit often, almost as often as you save the files you're working
  on.
- Push often, at least several times per session. This makes sure that
  your "local" edits are synced with your "remote" repository on GitHub.
- When you sign in to RStudio server to start a new session first pull
  edits from GitHub (click downward arrow in the rightmost upper
  panel). This makes sure that if you've worked on files directly on
  GitHub these edits will be merged with your local files.

## Next steps

Congratulations, you should now be ready to write some project code of
your own! To get started, read the vignette "Developing with
rofi". To see all vignettes in this package, run:

```{r, eval = FALSE}
browseVignettes("rofi")
```

And to open a specific vignette, run:

```{r, eval = FALSE}
vignette("developing", "rofi")