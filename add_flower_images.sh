#!/bin/bash
# Add parametric flower images to all markdown files

echo "🌸 Adding parametric flower images to all markdown files..."

# Find all markdown files except the main README.md
find . -name "*.md" -type f | grep -v "^./README.md$" | while read -r file; do
    echo "Processing: $file"
    
    # Check if the file already has the image
    if ! grep -q "parametric-flower-compressed.png" "$file"; then
        # Add the image after the first heading
        sed -i '' '1a\
![Parametric Flower](parametric-flower-compressed.png)\
' "$file"
        echo "  ✅ Added parametric flower image"
    else
        echo "  ⏭️  Already has parametric flower image"
    fi
done

echo "🌸 All markdown files updated with parametric flower images!"
