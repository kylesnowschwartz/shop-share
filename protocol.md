# Protocol


## Register

Client -> Server
```json
{
  "action": {
    "type": "Register"
  }
}
```

Server -> Client
```json
{
  "confirmAction": {
    "type": "Register",
    "clientId": "1"
  }
}
```


## GetLists

Client -> Server
```json
{
  "action": {
    "type": "GetLists"
  }
}
```

Server -> Client
```json
{
  "confirmAction": {
    "type": "GetLists",
    "lists": [
      {
        "listId": "1",
        "title": "foo"
      }
    ]
  }
}
```
