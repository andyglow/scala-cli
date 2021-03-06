- [x] quoted string support
- [x] add testcases for quoted args/opts for Recognizer
- [x] full line parser. Tokenizer
- [x] allow option to use model2model transformations instead of Res.Cmd. 
      custom case class, structures
      ideally Res.Cmd should be build by using this approach
      - [x] for that we need to modify the way we validate varargs (now it depends on Res.Cmd). 
      Instead we will probably have to put some other tracking class into the scope, or add tracking field directly 
      into VarArg that will be decreased each time we handle vararg and checked at the end.
      
      Resulted in `Res.Cmd`   
- [x] POSIX-style short option names (-a) with grouping (-abc)      
- [ ] implement 2-col layout for Usage
- [ ] add `examples` for command to allow user to specify examples that would be printed out with the Usage
- [ ] implement colorful renderer for Usage 
- [ ] implement macro-derivation
- [x] support Property arguments (-Dkey1=value1 -Dkey2=value2) for Maps
- [x] support for repetitive opts: `-l=debug -l=info` for Lists
- [x] support for repetitive flags: `-v -v -v`
- [ ] support for negative flags. if there is a flag defined as `--color`, it would make sense to handle as well a flaf `--no-color`
- [x] custom effect instead of either (+ dsl)
- [ ] ignore or fail on unknown properties behaviour
- [ ] update usage renderer with Opt/Flag info regarding repetitive-ness and kv-ness
- [ ] README.md
- [ ] article on medium.com  