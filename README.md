# mergeJsonRequest

A tool for merge json api request


# Usage

```
type Query {
  request(uri: String!, method: String, headers: Object, body: String): Value
}

type Value {
  err: String
  .
  .
  .
}
```

```bash
echo '{ request(uri: "/abcd") }' | mergeJsonRequest https://gw.huabot.com
```
