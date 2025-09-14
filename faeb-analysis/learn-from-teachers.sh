#!/bin/bash
# Learn from Teachers - Video Analysis Script 💙

echo "🦉 Welcome to the Wise Owl - Learning from Teachers 💙"
echo "=================================================="
echo ""
echo "We are about to listen to other creators talk about how they make
beautiful things."
echo "We are not copying - we are learning how others breathe."
echo ""

# Create directories for our learning
mkdir -p teachers/audio
mkdir -p teachers/transcripts

# List of teachers to learn from
TEACHERS=(
    "https://www.youtube.com/watch?v=NdNUI6Zh-pA"
    "https://www.youtube.com/watch?v=8Hoklh6e4xk"
    "https://www.youtube.com/watch?v=qjf77R7mKVw"
    "https://www.youtube.com/watch?v=mSWE4JAVZMk"
    "https://www.youtube.com/watch?v=dI17qo42AcY"
    "https://www.youtube.com/watch?v=ciAWQBZMJoo"
    "https://www.youtube.com/watch?v=WQkl6K_8Mpo"
)

echo "🎓 Found ${#TEACHERS[@]} teachers to learn from:"
for i in "${!TEACHERS[@]}"; do
    echo "   $((i+1)). ${TEACHERS[$i]}"
done

echo ""
echo "💙 Ready to begin learning from these teachers!"
echo "🦉 The wise owl will help us gather wisdom from other creators."
echo ""
echo "Next steps:"
echo "1. Download audio from each teacher"
echo "2. Listen to their breathing discipline"
echo "3. Learn how they create beautiful things"
echo "4. Apply their wisdom to our own lotus flower growing"
echo ""
echo "Remember: We are not copying - we are learning how others breathe! 💙"
