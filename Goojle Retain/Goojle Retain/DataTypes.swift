//
//  DataTypes.swift
//  Goojle Retain
//
//  Created by Timothy Barraclough on 18/08/17.
//  Copyright © 2017 Shop Share Industries. All rights reserved.
//

import Foundation

struct ServerResponse : Decodable {

    let confirmAction  : ConfirmAction

    struct ConfirmAction : Decodable {

        let type : type
        let data : Data

        enum type : String, Decodable {
            case Register
            case GetLists
            case CreateList
            case DeleteList
            case UpdateListTitle
            case CreateItem
        }

        struct Data : Decodable {
            let clientId : Int?
            let lists : [List]?
        }
    }
}

struct List : Codable {
    let listId : Int? = nil
    let title : String
    let items : [Item]
}

struct Item : Codable {
    let itemId : Int? = nil
    let text : String
    let completed : Bool
}
