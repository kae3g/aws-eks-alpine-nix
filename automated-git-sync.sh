#!/bin/bash
# Automated Git Sync for b122m faeb Gentle Revolution
# Commits and pushes to both branches every 5 minutes

echo "🌸 Starting Automated Git Sync for b122m faeb Gentle Revolution"
echo "🔄 Committing and pushing to both branches every 5 minutes"
echo "📅 Started at: $(date)"
echo "💙 Repository: aws-eks-alpine-nix"
echo "🌿 Branches: main, druid-hills"
echo "=========================================="

# Function to commit and push to both branches
sync_branches() {
    local timestamp=$(date "+%Y-%m-%d--%H%M--pacific-standard-usa")
    
    echo "🔄 Sync cycle started at: $timestamp"
    
    # Check for changes
    if git diff --quiet && git diff --cached --quiet; then
        echo "📝 No changes detected, updating progress only"
        
        # Update PROGRESS.md with current timestamp
        echo "" >> docs/PROGRESS.md
        echo "**Automated Sync**: $timestamp - Repository synchronized across branches" >> docs/PROGRESS.md
        
        git add docs/PROGRESS.md
        git commit -m "Cursor: Automated sync - $timestamp

🔄 Automated Git Sync:
- Updated PROGRESS.md with sync timestamp
- Repository synchronized across both branches
- No code changes detected
- System maintaining gentle revolution continuity

Timestamp: $timestamp
Status: Automated maintenance sync
Philosophy: b122m faeb gentle revolution" || echo "⚠️  No changes to commit"
    else
        echo "📝 Changes detected, committing and pushing"
        
        # Add all changes
        git add .
        
        # Commit with detailed message
        git commit -m "Cursor: Automated sync with changes - $timestamp

🔄 Automated Git Sync with Changes:
- Repository changes detected and committed
- Synchronized across both main and druid-hills branches
- Maintaining gentle revolution continuity
- All changes preserved with b122m faeb philosophy

Timestamp: $timestamp
Status: Automated sync with changes
Philosophy: b122m faeb gentle revolution"
    fi
    
    # Push to main branch
    echo "📤 Pushing to main branch..."
    git push origin main
    
    # Switch to druid-hills and merge
    echo "🌿 Switching to druid-hills branch..."
    git checkout druid-hills
    git merge main
    
    # Push to druid-hills
    echo "📤 Pushing to druid-hills branch..."
    git push origin druid-hills
    
    # Switch back to main
    git checkout main
    
    echo "✅ Sync cycle completed at: $(date)"
    echo "⏰ Next sync in 5 minutes..."
    echo "----------------------------------------"
}

# Main loop - run every 5 minutes
while true; do
    sync_branches
    sleep 300  # 5 minutes = 300 seconds
done
