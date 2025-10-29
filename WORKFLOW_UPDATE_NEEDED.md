# Manual Workflow Update Required

## Issue
The GitHub Actions workflow `.github/workflows/test.yml` needs to be updated with additional permissions, but GitHub Apps cannot directly modify workflow files for security reasons.

## Error Being Fixed
```
RequestError [HttpError]: Resource not accessible by integration (403)
```

This error occurs when the test workflow tries to post coverage comments on PRs but lacks the necessary permissions.

## Solution

### Option 1: Copy the Updated Template (Recommended)
```bash
cp workflows-to-install/test.yml .github/workflows/test.yml
git add .github/workflows/test.yml
git commit -m "Update test workflow with PR comment permissions"
git push
```

### Option 2: Manual Edit
Add the following permissions block to `.github/workflows/test.yml` after line 15:

```yaml
jobs:
  test:
    name: Run Tests & Check Coverage
    runs-on: ubuntu-latest
    permissions:           # <-- ADD THESE LINES
      contents: read       # <-- ADD THESE LINES
      pull-requests: write # <-- ADD THESE LINES
      issues: write        # <-- ADD THESE LINES
```

Also add `continue-on-error: true` to the "Comment coverage on PR" step (around line 118):

```yaml
      - name: Comment coverage on PR
        if: github.event_name == 'pull_request' && always()
        continue-on-error: true  # <-- ADD THIS LINE
        uses: actions/github-script@v7
```

## What This Fixes
- Allows the workflow to post coverage reports as PR comments
- Prevents the workflow from failing if permissions are misconfigured
- Maintains security by explicitly declaring required permissions

## After Updating
The test workflow will be able to:
- ✅ Post coverage reports on PRs
- ✅ Update issue comments
- ✅ Continue even if commenting fails (graceful degradation)
