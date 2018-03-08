# org

This repository contains company-wide guidelines on writing code and organizing
workflow. If Kowainik member don't know how some part of his/her work should be
he/she should refer to this guide. If some part of workflow is not clear from
this document — open issue to this repository. But, of course, everyone should
read this README first.

## Workflow

### Version control system

Git version control system based on GitHub is used.

> TODO: experiment with Pijul at some moment in future.

#### Issues and branches

1. Work should be done under issue. If such issue doesn't exist: create it.
2. Always discuss work under issue before doing this issue. If you see multiple
   ways to do something — discuss with somebody under issue comments section how
   to do this. If you're not sure that there's only one way to do the thing —
   discuss with somebody under issue comments section how to do this. In perfect
   case scenario each issue should contain some plan how to do this issue.
3. `master` branch should be stable, always compilable and working.
4. Do your work for some issue under separate branch forked from `master` branch.
5. Name your branch as `username/issueId-short-description`. For example: `chshersh/1-describe-workflow`.
6. You _shouldn't_ fix multiple issues in a single branch.
7. Use [this guide](https://chris.beams.io/posts/git-commit/) for commit messages.
8. Every commit should contain issue number in square brackets before message.
   * Example: `[#6] Describe basic guidelines`

#### Pull requests

1. Open PR if you:
   * Finished
   * Want to share work with others
2. Add `wip` label to your PR if work is not finished.
3. Add issue number at the title of PR in square brackets (just like in commit messages).
4. Request review from at least two people in team.
5. Pull request can be merged to `master` only if there're two approvals for this PR.
6. Resolve all conflicts using only `git rebase` command to make history clean.

### Project structure

1. Use `cabal` build tool with `cabal new-build` or `stack` as a main build
   tool. But it's desired to support both of them on CI.
2. Every project should contain the following files in addition to code:
   * README.md
   * CHANGES.md
   * `.stylish-haskell.yaml`
   * `.travis.yml`
3. Use PVP versioning for projects.

> **TODO:** What about Windows CI?

> **TODO:** default hs-init configuration for company project template?

## Code style

1. Use [Serokell Style guide](https://github.com/serokell/serokell-util/blob/c7e71ac4684a5bf345fd52c9656fb9cdf4e03f5d/.stylish-haskell.yaml)
   for Haskell code.
2. Use `stylish-haskell` to format code.
3. Use [`universum`](https://github.com/serokell/universum) custom prelude for
   applications or huge libraries.
