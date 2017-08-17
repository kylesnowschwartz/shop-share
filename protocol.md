# WS Protocol


## Errors

Server -> Client
```json
{
  "error": {
    "message": "Action not yet built. Sorry! Come back later :-)"
  }
}
```


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
    "data": {
      "clientId": 1
    }
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
    "data": {
      "lists": [
        {
          "id": 1,
          "title": "Test shopping list"
        }
      ]
    }
  }
}
```

## CreateList

* TODO: What ids should the clients give lists before they're persisted?
  - UUIDs on the client, then DB ids after persisted

Client -> Server
```json
{
  "action": {
    "type": "CreateList",
    "title": "Foo bar list"
  }
}
```

Server -> Client
```json
{
  "confirmAction": {
    "type": "CreateList",
    "list": {
      "listId": 1,
      "title": "foo"
    }
  }
}
```
