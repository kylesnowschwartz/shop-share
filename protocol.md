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
          "listId": 1,
          "title": "Test shopping list",
          "items": [
            {
              "itemId": 1,
              "text": "Get some Haskell down ya",
              "completed": false
            }
          ]
        }
      ]
    }
  }
}
```


## CreateList

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
    "data": {
      "list": {
        "items": [],
        "listId": 12,
        "title": "New fucking list, brah!"
      }
    }
  }
}
```


## DeleteList

Client -> Server
```json
{
  "action": {
    "type": "DeleteList",
    "listId": 9
  }
}
```

Server -> Client
```json
{
  "confirmAction": {
    "type": "GetLists",
    "data": {
      "lists": [{
        "items": [],
        "listId": 1,
        "title": "Test shopping list"
      }]
    }
  }
}
```


## UpdateListTitle

Client -> Server
```json
{
  "action": {
    "type": "UpdateListTitle",
    "listId": 1,
    "title": "New title"
  }
}
```

Server -> Client
```json
{
  "confirmAction": {
    "type": "UpdateListTitle",
    "data": {
      "list": {
        "items": [],
        "listId": 1,
        "title": "New title"
      }
    }
  }
}
```

## CreateItem

Client -> Server
```json
{
  "action": {
    "type": "CreateItem",
    "text": "",
    "listId": 1
  }
}
```

Server -> Client
```json
{
  "confirmAction": {
    "type": "CreateItem",
    "data": {
      "item": {
        "listsId": 1,
        "text": "item for list 1",
        "itemId": 3,
        "completed": false
      }
    }
  }
}
```
