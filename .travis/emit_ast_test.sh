python ekcc.py -emit-ast -o test/test1.ek.ast.yaml test/test1.ek
python ekcc.py -emit-ast -o test/test2.ek.ast.yaml test/test2.ek
python -c "import sys,yaml;sys.exit(yaml.load(open('test/test1.ast.yaml'))!=yaml.load(open('test/test1.ek.ast.yaml')) or yaml.load(open('test/test2.ast.yaml'))!=yaml.load(open('test/test2.ek.ast.yaml')))"
