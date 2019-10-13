Spark data sources are easy to make yourself. Today we are going to create the simplest possible spark source. 

Our data source produces a list of 3 results with incremental id's. We will be able to read from the data source like so:

``` scala
val df = spark
  .read
  .format("SimpleSource")
  .load()

df.show()
```

Which prints out:

```
+---+
| id|
+---+
|  1|
|  2|
|  3|
+---+
```


To create our own reader we are going to make use of the [`DataSourceV2`](https://spark.apache.org/docs/2.4.0/api/scala/#org.apache.spark.sql.sources.v2.DataSourceV2) api which was introduced in Spark 2.3 and updated slightly in 2.4. 

There are many great articles explaining all the great things you can do with the api like [all](https://databricks.com/session/apache-spark-data-source-v2) [of](http://blog.madhukaraphatak.com/spark-datasource-v2-part-1/) [these](https://developer.ibm.com/code/2018/04/16/introducing-apache-spark-data-sources-api-v2/) [ones](https://issues.apache.org/jira/browse/SPARK-15689). I'm not going to go very deep, we are just building the *minimal* example. 

Our data source is very simple. The only thing it supports is reading.

```scala
class SimpleSource extends DataSourceV2 with ReadSupport {
  def createReader(options: DataSourceOptions): DataSourceReader =
    new SimpleSourceReader
}
```

Whenever we read a row from the data source it will have the one column named `id`. We want to read sequentially from a single source so we are only going to have a single partition.

```scala
class SimpleSourceReader extends DataSourceReader {
  def makePartition: InputPartition[InternalRow] =
    new SimpleInputPartition

  def planInputPartitions: java.util.List[InputPartition[InternalRow]] =
    Seq(makePartition).asJava

  def readSchema: StructType =
    StructType(StructField("id", IntegerType) :: Nil)
}
```

We do need to supply a bit of plumbing to work with the system spark internals. This is basically the [factory pattern](https://en.wikipedia.org/wiki/Factory_method_pattern).

```scala
class SimpleInputPartition extends InputPartition[InternalRow] {
  def createPartitionReader: InputPartitionReader[InternalRow] =
    new SimpleInputPartitionReader
}
```

And finally we get to to the the interesting bit. We read from a source similar to [observable stream](http://reactivex.io/documentation/observable.html). Whenever Spark calls the get function, we return the current row. Whenever Spark calls the next function we move to the next row. If we have finished consuming the stream, we return false.

```scala
class SimpleInputPartitionReader extends InputPartitionReader[InternalRow] {
  var i: Integer = 0

  def get = InternalRow(i)
  def close = Unit

  def next = if (i < 3) {
    i += 1; true
  } else {
    false
  }
}
```

And thats it, now we have our very own custom spark data source!
