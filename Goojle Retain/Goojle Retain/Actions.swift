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

    func toString() -> String {
        switch self {
        case .register:
            return "{\"action\": {\"type\": \"Register\"}}"
        }
    }

}

enum Server {
    case register(response : RegisterResponse)
    case error(error : SocketError)
}

struct RegisterResponse {

}

struct SocketError {

}
