# WS Protocol


## Errors

Server -> Client
```js
{
  "error": {
    "message": "Action not yet built. Sorry! Come back later :-)"
  }
}
```


## Register

Client -> Server
```js
{
  "action": {
    "type": "Register",
    "data": {}
  }
}
```

Server -> Client
```js
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
```js
{
  "action": {
    "type": "GetLists",
    "data": {}
  }
}
```

Server -> Client
```js
{
  "confirmAction": {
    "type": "GetLists",
    "data": {
      "lists": [
        {
          "listId": "55a82a71-1d8a-4abd-9aa1-29b351766c48",
          "title": "Test shopping list",
          "items": [
            {
              "itemId": "5835ac49-559f-42d6-b9b1-c3695fe8fa61",
              "text": "Get some Haskell down ya",
              "completed": false,
              "listId": "55a82a71-1d8a-4abd-9aa1-29b351766c48"
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
```js
{
  "action": {
    "type": "CreateList",
    "data": {
      "listId": "55a82a71-1d8a-4abd-9aa1-29b351766c48"
    }
  }
}
```

Server -> Client
```js
{
  "confirmAction": {
    "type": "CreateList",
    "data": {
      "lists": [ ... ]
    }
  }
}
```


## UpdateListTitle

Client -> Server
```js
{
  "action": {
    "type": "UpdateListTitle",
    "data": {
      "listId": "55a82a71-1d8a-4abd-9aa1-29b351766c48",
      "title": "New title"
    }
  }
}
```

Server -> Client
```js
{
  "confirmAction": {
    "type": "UpdateListTitle",
    "data": {
      "lists": [ ... ]
    }
  }
}
```


## DeleteList

Client -> Server
```js
{
  "action": {
    "type": "DeleteList",
    "data": {
      "listId": "55a82a71-1d8a-4abd-9aa1-29b351766c48"
    }
  }
}
```

Server -> Client
```js
{
  "confirmAction": {
    "type": "GetLists",
    "data": {
      "lists": [ ... ]
    }
  }
}
```

## CreateItem

Client -> Server
```js
{
  "action": {
    "type": "CreateItem",
    "data": {
      "item": {
        "itemId": "51f66427-48a4-4495-b778-f956b4f23754",
        "text": "",
        "completed": false,
        "listId": "5f74b6c4-2ac6-44d3-abfa-1757d7a2d3be"
      }
    }
  }
}
```

Server -> Client
```js
{
  "confirmAction": {
    "type": "CreateItem",
    "data": {
      "lists": [ ... ]
    }
  }
}
```

## UpdateItem

Client -> Server
```js
{
  "action": {
    "type": "UpdateItemText",
    "data": {
      "item": {
        "itemId": "51f66427-48a4-4495-b778-f956b4f23754",
        "text": "New text",
        "completed": true,
        "listId": "5f74b6c4-2ac6-44d3-abfa-1757d7a2d3be"
      }
    }
  }
}
```

Server -> Client
```js
{
  "confirmAction": {
    "type": "UpdateItemText",
    "data": {
      "lists": [ ... ]
    }
  }
}
```


# TODO:
## CompleteItem
## UncompleteItem
## DeleteItem
