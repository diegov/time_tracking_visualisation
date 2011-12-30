find -maxdepth 1 -type f -iname "*\.test\.js" | while read f; do 
    echo "Running tests from $f"
    js -opt -1 rhino_runner.js "$f"; 
done