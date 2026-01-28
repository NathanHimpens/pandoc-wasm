# Git Create Worktree

## Overview

Create a new working tree (a separate directory) linked to the current repository. This allows you to work on multiple branches simultaneously without stashing changes or switching contexts in your main directory.

## Steps

1. **Identify the target branch**
    - Decide if you are checking out an **existing branch** or creating a **new one**
    - List existing branches: `git branch` to ensure you have the correct name
2. **Determine the destination path**
    - Choose a location outside your current working directory (usually one level up)
    - Example: `../my-project-feature-fix`
3. **Run the creation command**
    - **Existing branch:** `git worktree add <path> <branch>`
    - **New branch:** `git worktree add -b <new-branch> <path> <base-branch>`
4. **Initialize environment**
    - `cd` into the new directory
    - Re-install dependencies (e.g., `npm install`) as these are not shared between worktrees

## Template

- **Existing branch:** `git worktree add ../<folder-name> <existing-branch>`
- **New branch:** `git worktree add -b <new-branch-name> ../<folder-name> <origin-branch>`

## Rules

- **Unique branches:** You cannot check out a branch that is already active in another worktree
- **Path management:** Always place worktrees outside the main project folder to avoid tracking them as untracked files
- **Clean removal:** Do not just delete the folder; use `git worktree remove <path>` to keep the Git metadata clean
- **Independence:** Remember that while the code is linked, local environment files (`.env`, `node_modules`) are unique to each worktree
