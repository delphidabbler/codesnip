# Contributing

Thanks for you interest in contributing to the CodeSnip project.

## Contents

* [Overview](#overview)
* [Issues](#issues)
* [Documentation](#documentation)
* [Coding](#coding)
* [About Pull Requests](#about-pull-requests)
* [Licensing of Contributions](#licensing-of-contributions)
* [Code of Conduct](#code-of-conduct)
* [Attributions](#attributions)

## Overview

Contributions of all kinds are more than welcome.

> 💡 You will need a GitHub account to be able to make contributions. [Sign up here](https://docs.github.com/en/get-started/signing-up-for-github/signing-up-for-a-new-github-account).

There three main ways in which you can contribute, each of which is explained in its own section below. From easiest to hardest, they are:

* [Issues](#issues) - report bugs and request new features.
* [Documentation](#documentation) - improve the project's documentation.
* [Coding](#coding) - modify the project's source code.

Regardless of how you choose to contribute, please respect the project's [code of conduct](#code-of-conduct).

## Issues

The easiest way to make a contribution is to [create an issue](https://github.com/delphidabbler/codesnip/issues/new).

You can use issues to:

1. [Report a bug](#reporting-a-bug)
2. [Request a new program feature](#requesting-a-new-feature)
3. [Suggest changes to documentation](#suggesting-documentation-changes)

It is helpful if you perform a cursory search of [existing issues](https://github.com/delphidabbler/codesnip/issues?q=is%3Aissue) to see if there is already a similar issue. If so then please add your thoughts as comments on that issue rather than open a new one.

### Reporting a bug

Before reporting a bug please make sure the bug exists in the [latest release](https://github.com/delphidabbler/codesnip/releases) of CodeSnip 4.

To report a bug please provide as much information about the bug as possible, including the program's version number, what happened, what you expected to happen and what you were doing at the time. If possible, explain how to reproduce the bug.

> 💡 You can find CodeSnip's version number in the program's About box (_Help | About_ menu option).

If you have resolved an issue yourself please consider contributing your fix so that others can benefit from your work. Please read the [Coding](#coding) section before doing so.

### Requesting a new feature

If you think of a feature that you would like to see added to CodeSnip you can open an issue to request it.

Please be as clear as possible about what you expect the feature to do and why you want it.

Ideas about how to implement the feature are welcome. Even better, if you have implemented the feature yourself, you may be able to contribute the code. But please read the [Coding](#coding) section before doing so.

### Suggesting documentation changes

The third reason to create an issue is if you want to suggest new documentation or ammendments to existing documentation.

There are two types of documentation within the CodeSnip repository. Firstly, there is general documentation that can be found in the [repository root](https://github.com/delphidabbler/codesnip/tree/develop) and in the [`Docs`](https://github.com/delphidabbler/codesnip/tree/develop/Docs) directory. Secondly, there are numerous HTML help files in the [`Src/Help/HTML`](https://github.com/delphidabbler/codesnip/tree/develop/Src/Help/HTML) directory.

If you have written or corrected some documentation yourself please consider submitting it. See the [Documentation](#documentation) section below to find out how to do this.

## Documentation

Writing and editing documentation is a relatively easy way to start contributing. Providing simple clear or helpful documentation for users is critical. Things that *you* found hard to understand as a user, or difficult to work out, are excellent places to begin.

There are two ways to contribute documentation. The preferred method is by means of a [pull request](#about-pull-requests). But, you can simply create an issue and attach documentation files to the issue comments. This approach is perfectly acceptable for a small number of files, and saves you having to install Git!

## Coding

Code contributions to the [CodeSnip 4 development tree](https://github.com/delphidabbler/codesnip/tree/develop) are always welcome, from fixing bugs to developing new features.

> 🐘 Before going any further, let's address the elephant in the room. CodeSnip has to be compiled with the now rather ancient Delphi XE. It would be much better if development could move to a more recent version of Delphi, but that's proving to be extremely problematic. See [this FAQ](https://github.com/delphidabbler/codesnip-faq/blob/master/SourceCode.md#faq-11) for an explanation.

If you're still here, you will need to set up a valid build environment in order to compile CodeSnip from source. [`Build.html`](https://htmlpreview.github.io/?https://github.com/delphidabbler/codesnip/blob/develop/Build.html) tells you everything you need to know about configuring the environment, the tools you will need, and how compile CodeSnip using the provided `Makefile`.

> 💡 Need ideas? You can [browse open issues](https://github.com/delphidabbler/codesnip/issues) and see if you can help with any of them. 

Contributions should normally be made using [pull requests](#about-pull-requests).

But, if you have just a few lines of code to suggest it _may_ be possible to accept the code within (or attached to) a comment on an issue. If in doubt, ask first by means of a comment. When including the code in a comment, please use [GitHub code fencing](https://docs.github.com/en/get-started/writing-on-github/working-with-advanced-formatting/creating-and-highlighting-code-blocks) to format the code and make it easier to read. For example:

    ```pascal
    procedure TFoo.CoolNewMethod;
    begin
      // cool code here
    end;
    ```

## About Pull Requests

All pull requests *must* relate to an [open _accepted_ issue](https://github.com/delphidabbler/codesnip/issues?q=is%3Aissue+label%3Aaccepted++is%3Aopen+) on GitHub.

If you wish to contribute code for which there is no existing issue you can create one. Please wait for the issue to be accepted by the project owner before submitting your pull request. Acceptance is indicated by attaching an "accepted" label to the issue and is signed off by means of a comment on the issue from the project owner.

> 💡 Should you get stuck at any point please ask for help by leaving a comment on the  relevant issue.

Before you can start you need to get the source code. Here's how:

1. Fork the CodeSnip project into your own GitHub repository. 

    ⭐***Important***⭐ _Ensure that all remote branches are included in the fork_, not just `master`. On GitHub this means clearing the "Copy the `master` branch only" check box before creating the fork.

2. Clone the forked repository onto your own system using:
   
        $ git clone https://github.com/<account>/codesnip.git

    where `<account>` is your GitHub user account ID.

CodeSnip uses the [Git Flow](https://nvie.com/posts/a-successful-git-branching-model/) methodology. This means that you need to create your source code on a "feature" branch off the `develop` branch. Here's how:

1. Pull the `develop` branch from your forked repository, and switch into it:

        $ git pull origin develop
        $ git switch develop

2. Create and switch to a new feature branch, branched off `develop`.
    
    Name the branch `feature/` followed by the issue number to which it relates. Appending a hyphen followed by a brief description is useful too. Example branch names are  `feature/42-fix-ui-bug` or `feature/56`:

        $ git branch feature/42-fix-ui-bug
        $ git switch feature/42-fix-ui-bug

3. Make your changes, whether to source code or documentation, and commit them to your feature branch. Clarity is prefered to terseness in commit messages. Please don't squash logically unrelated commits.

4. If you are editing source code you need to ensure that it compiles and works as expected.

    > 💡 Having trouble? Please make sure you have read [`Build.html`](https://htmlpreview.github.io/?https://github.com/delphidabbler/codesnip/blob/develop/Build.html). If that doesn't solve your problem then ask for help by creating a comment on the relevant issue.

Once you have finished writing the code and are ready to submit it you need to prepare your forked repository:

1. Use GitHub to ensure that the `develop` branch of your fork is up to date with the parent `delphidabbler/codesnip` repository's `develop` branch.

2. Rebase your changes onto your forked repository's remote `develop` branch:

        $ git rebase origin/develop feature/42-fix-ui-bug

3. Resolve any conflicts arising from the rebase then push your changes to your forked repository:

        $ git push origin feature/42-fix-ui-bug

You are finally ready to open that pull request. Proceed as follows:

1. Open your forked repository on GitHub. You should see a banner saying something like "feature/42-fix-ui-bug had recent pushes ..." along with a button that says "Compare and Pull Request". Click that button.

2. You will be taken the "Comparing Changes" page on `delphidabbler/codesnip`.

    ⭐***Important***⭐ Just below the page title is a line of drop-down buttons where you choose what to compare. Check the caption of the 2nd button from the left. If it reads anything other than "base: `develop`" (and it probably doesn't) then click the button and select `develop` from the list of branches that is displayed.
    
    > ⚠️ Pull requests based on `master`, or any branch other than `develop`, will not be accepted.

3. Now complete the form and click the "Create Pull Request" button.

That's it.

Sit back and wait. Either your pull request will be accepted or you will receive comments requesting clarification or changes.

Once your pull request is accepted you can delete the feature branch from both your local and forked remote fork.

## Licensing of Contributions

The license that applies to any existing file you edit will continue to apply to the edited file. Any existing license text or copyright statement **must not** be altered or removed, but you can add your own copyright line if desired.

Any new file you contribute **must** either be licensed under the [Mozilla Public License v2.0](https://www.mozilla.org/MPL/2.0/) (MPL2) or have a license compatible with the MPL2. If a license is not specified then MPL2 will be assumed and will be applied to the file. You should insert a suitable copyright statement in the file.

Any third party code used by your contributed code **must** also have a license compatible with the MPL2.

> 💡 MPL2 boilerplate text, in several programming language's comment formats, can be found in the file [`Docs/MPL-2.0-Boilerplate.txt`](https://raw.githubusercontent.com/delphidabbler/codesnip/master/Docs/MPL-2.0-Boilerplate.txt). You will need to change the name of the copyright holder.

## Code of Conduct

This is a simple and straightforward code of conduct. What it boils down to is that if you're a decent person you're more than welcome here. Hateful, abusive and just plain rude people are not.

To be more precise, anyone who contributes to the CodeSnip project is expected to display respect, empathy and politeness towards others.

Certain behaviours will not be tolerated, and will result in an immediate ban. They are:

* Racism and hate speech in any form.
* The use of sexualized language or imagery, and sexual attention or advances of any kind.
* Intolerance towards people of any religion or none.
* Trolling, insulting or derogatory comments, and personal or political attacks.
* Public or private harassment.
* Publishing others' private information, such as a physical or email address, without their explicit permission.
* Other conduct which could reasonably be considered inappropriate in a professional setting.

## Attributions

1. The list of unacceptable behaviours in the Code of Conduct was taken, in part, from the "Our Standards" section of the [Contributor Covenant Code of Conduct v2.1](https://www.contributor-covenant.org/version/2/1/code_of_conduct/).

2. Some of the content of this document was inspired by and/or copied from the [Stumpy Contributing Guide](https://stumpy.readthedocs.io/en/latest/Contribute.html).