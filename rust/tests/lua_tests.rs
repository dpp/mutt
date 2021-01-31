use rlua::{Lua, Result};

#[test]
fn test_lua_mem() -> Result<()> {
    let mut v = vec![];

    for x in 0..1_000 {
        let lua = Lua::new();

        // In order to interact with Lua values at all, you must do so inside a callback given to the
        // `Lua::context` method.  This provides some extra safety and allows the rlua API to avoid some
        // extra runtime checks.
        lua.context(|lua_ctx| {
            // You can get and set global variables.  Notice that the globals table here is a permanent
            // reference to _G, and it is mutated behind the scenes as Lua code is loaded.  This API is
            // based heavily around sharing and internal mutation (just like Lua itself).

            let globals = lua_ctx.globals();

            globals.set("string_var", "hello")?;
            globals.set("int_var", 42)?;
            globals.set("loop_num", x)?;

            assert_eq!(globals.get::<_, String>("string_var")?, "hello");
            assert_eq!(globals.get::<_, i64>("int_var")?, 42);

            assert_eq!(lua_ctx.load("1 + 1").eval::<i32>()?, 2);
            assert_eq!(lua_ctx.load("false == false").eval::<bool>()?, true);
            assert_eq!(lua_ctx.load("return 1 + 2").eval::<i32>()?, 3);

            Ok(())
        })?;

        lua.context(|lua_ctx| {
            let globals = lua_ctx.globals();

            assert_eq!(globals.get::<_, String>("string_var")?, "hello");
            assert_eq!(globals.get::<_, i64>("int_var")?, 42);

            assert_eq!(lua_ctx.load("1 + 1").eval::<i32>()?, 2);
            assert_eq!(lua_ctx.load("false == false").eval::<bool>()?, true);
            assert_eq!(lua_ctx.load("return 1 + 2").eval::<i32>()?, 3);

            Ok(())
        })?;
        v.push(lua);
    }

    Ok(())
}

#[test]
fn test_lua_clone() -> Result<()> {
    let lua = Lua::new();
    lua.context(|ctx| {
        let thing = ctx.load(
            r#"--[[
   table.bininsert( table, value [, comp] )
   
   Inserts a given value through BinaryInsert into the table sorted by [, comp].
   
   If 'comp' is given, then it must be a function that receives
   two table elements, and returns true when the first is less
   than the second, e.g. comp = function(a, b) return a > b end,
   will give a sorted table, with the biggest value on position 1.
   [, comp] behaves as in table.sort(table, value [, comp])
   returns the index where 'value' was inserted
]]--
do
   -- Avoid heap allocs for performance
   function hello()
      return "hello"
   end

   function answer()
     return 42
   end

   -- defines a factorial function
    function fact (n)
      if n == 0 then
        return 1
      else
        return n * fact(n-1)
      end
    end

   local fcomp_default = function( a,b ) return a < b end
   function table.bininsert(t, value, fcomp)
      -- Initialise compare function
      local fcomp = fcomp or fcomp_default
      --  Initialise numbers
      local iStart,iEnd,iMid,iState = 1,#t,1,0
      -- Get insert position
      while iStart <= iEnd do
         -- calculate middle
         iMid = math.floor( (iStart+iEnd)/2 )
         -- compare
         if fcomp( value,t[iMid] ) then
            iEnd,iState = iMid - 1,0
         else
            iStart,iState = iMid + 1,1
         end
      end
      table.insert( t,(iMid+iState),value )
      return (iMid+iState)
   end
end"#,
        );
        thing.eval()?;
        let hello: String = ctx.load("hello()").eval()?;

        assert_eq!(hello, "hello");
        let answer: isize = ctx.load("answer()").eval()?;
        assert_eq!(answer, 42);
        let nothing: Result<()> = ctx.load("call_a_function_that_doesnt_exist()").eval();
        assert!(nothing.is_err());
        let fact: isize = ctx.load("fact(10)").eval()?;
        assert_eq!(fact, 3_628_800);
        let path = std::env::current_dir().unwrap();
        println!("The current directory is {}", path.display());

        let _do_it: () = ctx.load(r#"dofile("tests/test.lua")"#).eval()?;
        let r: f64 = ctx.load("twice(4.2)").eval()?;
        assert_eq!(r, 8.4);
        let r: isize = ctx.load("thing(0)").eval()?;
        assert_eq!(r, 42);
        Ok(())
    })?;
    Ok(())
}
