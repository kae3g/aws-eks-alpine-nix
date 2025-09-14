#!/bin/sh
# Automated git sync for sovereign repository
set -e

echo "🤖 Starting automated git sync..."
echo "💙 This will commit and push changes every 5 minutes"

while true; do
    # Check if we're in a git repository
    if [ ! -d .git ]; then
        echo "❌ Not in a git repository"
        exit 1
    fi

    # Get current timestamp
    TIMESTAMP=$(date +"%Y-%m-%d--%H-%M--pacific-standard-usa")
    
    # Check for changes
    if git diff --quiet && git diff --staged --quiet; then
        echo "⏭️  No changes to commit at $TIMESTAMP"
    else
        # Add all changes
        git add .
        
        # Create signed commit
        git commit -S -m "🤖 Automated sync: $TIMESTAMP" -m "Sovereign infrastructure maintenance"
        
        # Push to both branches
        git push origin main
        git push origin druid-hills
        
        echo "✅ Changes committed and pushed at $TIMESTAMP"
    fi
    
    # Wait 5 minutes
    sleep 300
done
