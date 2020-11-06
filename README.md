# org

This repository contains organization-wide guidelines on the processes within Kowainik.

## Communications

We can be contacted by email xrom.xkov@gmail.com.

You can also subscribe to our news in
[Twitter](https://twitter.com/kowainik) or
[Telegram](https://t.me/kowainik).

## Workflow

### Version control system

Git version control system based on GitHub is used.

#### Issues and branches

1. Work should be done under corresponding GitHub issue.
   If such issue doesn't exist: create it.
2. Always discuss work under corresponding GitHub issue before doing this issue.
   If you see multiple ways to do something — discuss with somebody under issue
   comments section how to do this. If you're not sure that there's only one way
   to do the thing — discuss with somebody under issue comments section how to
   do this. In perfect case scenario each issue should contain some plan how to
   do this issue.
3. The main branch should be stable, always compilable and working.
4. Do your work for some issue under separate branch checkout from the main branch.
5. Name your branch as `username/issueId-short-description`. For example: `chshersh/1-describe-workflow`.
6. You _shouldn't_ fix multiple issues in a single branch.
7. Use [this guide](https://chris.beams.io/posts/git-commit/) for commit messages.
8. Every meaningful commit should contain issue number in square brackets before message.
   * Example: `[#6] Describe basic guidelines`

   Small fix commits shouldn't contain the issue number
   * Example: `Fix the typo`

   If you don't change source code, only documentation, then it's recommended
   to add `[skip ci]` tag to the end of the commit.
   * Example: `Change README.md [skip ci]`

#### Pull requests

1. Open PR if you:
   * Finished your work on the issue
   * Want to share work with others (for example you can open PR if you need
     somebody to look and to get advice for continue working on it)
2. Add `wip` label to your PR if work is not finished.
3. Add issue number at the title of PR in square brackets (just like in commit messages).
4. Request review from at least two people in team.
5. Pull request can be merged to the main branch only if there're two approvals for this PR.
6. Resolve all conflicts using only `git rebase` command to make history clean.
7. If you're the second person who approves the PR you should click on the
   [`Squash and Merge`](https://help.github.com/articles/about-pull-request-merges/#squash-and-merge-your-pull-request-commits)
   button.

### Project structure

1. You can use locally `cabal` or `stack` as your main build tool. But
   it's desired to support both of them on CI.
2. Every project should contain the following files in addition to code:
   * README.md
   * CHANGELOG.md
   * `.stylish-haskell.yaml`
   * `.github/CODEOWNERS.md`
   * `.github/workflows/ci.yml` (GitHub Actions CI)
3. Use PVP versioning for projects.

The easiest way to create a project is to use [Summoner](https://github.com/kowainik/summoner)
tool [with our own settings](https://github.com/kowainik/org/blob/main/.summoner.toml).

## Code style

1. Use [Kowainik Style guide](https://kowainik.github.io/posts/2019-02-06-style-guide) for Haskell code.
2. Use [`stylish-haskell`](https://github.com/jaspervdj/stylish-haskell)
   with [`.stylish-haskell.yaml`](.stylish-haskell.yaml) to format code.
3. Use [`relude`](https://github.com/kowainik/relude) custom prelude for
   applications or huge libraries.
