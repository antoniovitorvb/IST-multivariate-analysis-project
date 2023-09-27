<p align="center">
    <img src="https://upload.wikimedia.org/wikipedia/pt/e/ed/IST_Logo.png" height="200">
    <h1 align="center">Multivariate Analysis Project</h1>
</p>

## Quick Git tutorial:

**Pre-requisites:** Have a GitHub account

1. Use [Git Installation Guide](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) according to you OS;
   
2. Now open your `Terminal` or `Git Bash` and enter:
    ```bash
    git config --global user.name "<your_github_username>"

    git config --global user.email "your_github_email@example.com"
    ```

3. [Generate a New SSH Key and Add it to your GitHub](https://www.youtube.com/watch?v=X40b9x9BFGo)

4. Once you have Git installed and SSH key linked to your GitHub account, open your `Terminal` or `Git Bash` and enter:
    ```bash
    # Download the MA Project repository to your machine
    git clone git@github.com:antoniovitorvb/IST-multivariate-analysis-project.git
    ```
---

**Simple Git commands:**

1. Creates a new branch for you to work
```bash
git checkout -b ＜new-branch＞

# If you want to create a new branch copying from a specific one use
git checkout -b ＜new-branch＞ specific-branch＞

# To check which branch you're working on
git branch
```

2. See the status of the changes you've made
```bash
git status
```

3. Add new files you've created
```bash
git add . # all new files

git add <file_name> # add a specific file
```

4. Add your changes to the Commit log
```bash
git commit -m 'always leave some message'

# If you already added a file, but modified it, used the [-a] flag to add and commit together
git commit -am 'adds and commits modified files'
```

5. Updates your local folder with all changes that happened on the GitHub repo
```bash
git pull
```
`Obs.:` It is a good practice to use the `git pull` command before working on your thing


6. Upload all your changes to the GitHub repository
```bash
git push
```

**`BEWARE:` Avoid commiting and pushing to the `main` branch !!**