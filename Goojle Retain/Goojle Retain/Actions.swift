//
//  Actions.swift
//  Goojle Retain
//
//  Created by Timothy Barraclough on 17/08/17.
//  Copyright © 2017 Shop Share Industries. All rights reserved.
//

import Foundation

enum ClientAction {
    case register

    func toJSONString() -> String {
        switch self {
        case .register:
            return "{\"action\": {\"type\": \"Register\"}}"
        }
    }

}

enum ServerAction {
    case registered(ServerResponse)

    static let decoder = JSONDecoder()

    static func fromJSON(_ json: Data) -> Result<ServerAction> {
        if let response = try? decoder.decode(ServerResponse.self, from: json) {
            return .success(.registered(response))
        }
        return .error("Cannot get confirm action from data")
    }
}

struct ServerResponse : Decodable {

    struct ConfirmAction : Decodable {

        let type : String

        struct Data : Decodable {

            let clientId : Int
        }
    }

}

//struct SocketError {
//
//}

