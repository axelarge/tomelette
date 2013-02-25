TOML parser for Scala 2.10
===

## Compatibility
Seems to work fine with `00f11b019406531c8c7989846b1c1a54e9b8d8bb`

## Usage
```scala
val parser = new com.axelarge.tomelette.TomlParser

println(parser.parse("[some.toml] hi = 32"))
// Right(TDict(Map(some -> TDict(Map(toml -> TDict(Map(hi -> TLong(32))))))))

parser.parse(new FileReader("testdata/test.toml")).right.foreach { toml =>
  println(toml.get("owner.organization")) // Some(TString("GitHub"))
  println(toml.getAs[String]("servers.alpha.ip")) // Some("10.0.0.1")
  println(toml.getAs[Map[_,_]]("servers.beta")) // Some(Map(ip -> "10.0.0.2", dc -> "eqdc10"))
  println(toml.get("servers.beta")) // Some(TDict(Map(ip -> TString("10.0.0.2"), dc -> TString("eqdc10"))))
  println(toml.get("foo.blarg")) // None
}
```

##TODO
* Fix the mess that is string unescaping
* Actually make a jar
* Incremental parsing
* Type-safe accessors
* etc