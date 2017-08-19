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
    "type": "Register"
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
    "type": "GetLists"
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
```js
{
  "action": {
    "type": "CreateList",
    "title": "Foo bar list"
  }
}
```

Server -> Client
```js
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
```js
{
  "action": {
    "type": "DeleteList",
    "listId": 9
  }
}
```

Server -> Client
```js
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
```js
{
  "action": {
    "type": "UpdateListTitle",
    "listId": 1,
    "title": "New title"
  }
}
```

Server -> Client
```js
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
```js
{
  "action": {
    "type": "CreateItem",
    "text": "",
    "listId": 1
  }
}
```

Server -> Client
```js
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

## UpdateItemText

Client -> Server
```js
{
  "action": {
    "type": "UpdateItemText",
    "text": "foo bar",
    "itemId": 1
  }
}
```

Server -> Client
```js
{
  "confirmAction": {
    "type": "UpdateItemText",
    "data": {
      "item": {
        "listsId": 1,
        "text": "foo bar",
        "itemId": 1,
        "completed": false
      }
    }
  }
}
```


# TODO:
## CompleteItem
## UncompleteItem
## DeleteItem
