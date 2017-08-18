//
//  MockWebSocket.swift
//  Goojle Retain
//
//  Created by Timothy Barraclough on 18/08/17.
//  Copyright © 2017 Shop Share Industries. All rights reserved.
//

import Foundation

struct MockWebSocket : GRWebSocketDelegateProtocol {
    func connected() {
        send(.register)
    }

    func receivedError(_ error: GRError) {
        
    }

    let mockRegistration : Data = "{\"confirmAction\": {\"type\": \"Register\",\"data\": {\"clientId\": 1}}}".data(using: .utf8)!

    func send(_ action: ClientAction) {
        switch action {
        case .register:
            let action = ServerAction.fromJSON(mockRegistration)
            switch action {
            case .success(let act):
                received(act)
            case .error(_):
                break
            }
        case .getLists:
            break
        case .createList(_):
            break
        }
    }

    func received(_ action: ServerAction) {
        switch action {

        case .registered( let act ):
            dump(act)
        case .gotLists( let act):
            dump(act)
        case .createdList(let act):
            dump(act)
        }
    }
}
