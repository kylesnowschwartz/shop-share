//
//  ClientAction.swift
//  Goojle Retain
//
//  Created by Timothy Barraclough on 17/08/17.
//  Copyright © 2017 Shop Share Industries. All rights reserved.
//

import Foundation

enum ClientAction {
    case register
    case getLists
    case createList(String)

    func toJSONString() -> String {
        switch self {
        case .register:
            return "{\"action\": {\"type\": \"Register\"}}"
        case .getLists:
            return "{\"action\": {\"type\": \"GetLists\"}}"
        case .createList(let name):
            return "{\"action\": {\"type\": \"CreateList\",\"title\": \"\(name)\"}}"
        }
    }
}
