func foo() {
  var i = 0
  var somecomp = { print(i) } 
  i = 1
  somecomp()  // will see the new value
}

foo()
