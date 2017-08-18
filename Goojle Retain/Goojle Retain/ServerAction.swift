//
//  ServerAction.swift
//  Goojle Retain
//
//  Created by Timothy Barraclough on 18/08/17.
//  Copyright © 2017 Shop Share Industries. All rights reserved.
//

import Foundation

enum ServerAction {
    case registered(ServerResponse)
    case gotLists(ServerResponse)
    case createdList(ServerResponse)

    static let decoder = JSONDecoder()

    static func fromJSON(_ json: Data) -> Result<ServerAction> {
        if let response = try? decoder.decode(ServerResponse.self, from: json) {

            switch response.confirmAction.type {

            case .Register:
                return .success(.registered(response))
            case .GetLists:
                return .success(.gotLists(response))
            case .CreateList:
                return .success(.createdList(response))
            case .DeleteList:
                return .error(.unimplementedMethod)
            case .UpdateListTitle:
                return .error(.unimplementedMethod)
            case .CreateItem:
                return .error(.unimplementedMethod)
            }
        }
        return .error(.unableToParseJSON)
    }
}
