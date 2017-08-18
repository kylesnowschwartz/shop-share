//
//  GRWebSocket+Delegate.swift
//  Goojle Retain
//
//  Created by Timothy Barraclough on 18/08/17.
//  Copyright © 2017 Shop Share Industries. All rights reserved.
//

import Foundation

extension GRWebSocket : GRWebSocketDelegateProtocol {
    func receivedError(_ error: GRError) {
        print(error)
        switch error {
        case .textToDataError:
            break
        case .incorrectDataFormatError:
            break
        case .unimplementedMethod:
            break
        case .prematureDisconnection:
            break
        case .unableToParseJSON:
            break
        }
    }

    func connected() {
        delegate.send(.register)
    }

    func send(_ action: ClientAction) {
        let string = action.toJSONString()
        socket.write(string: string)
    }

    func received(_ action: ServerAction) {
        switch action {
        case .registered(_):
            print("Successfully registered!")
            send(.getLists)
        case .gotLists( let response):
            print("Recieved lists!")
            dump(response)
        case .createdList( let response ):
            print("Created List")
            print(response)
        }
    }
}
