@echo off
if [%1]==[] goto usage

@echo on
git add -u
git commit -m %1
git push

@echo off
goto finish

:usage

echo usage: git-commit.bat "Your commit message"

:finish
