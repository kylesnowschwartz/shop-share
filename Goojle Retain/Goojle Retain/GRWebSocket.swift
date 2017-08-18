//
//  WebSocket.swift
//  Goojle Retain
//
//  Created by Timothy Barraclough on 17/08/17.
//  Copyright © 2017 Shop Share Industries. All rights reserved.
//

import Foundation
import Starscream

class GRWebSocket : WebSocketDelegate, GRWebSocketDelegateProtocol {

    static let shared = GRWebSocket()

    fileprivate init() {
        socket = WebSocket(url: url)
        socket.delegate = self
        socket.connect()

    }

    func send(_ action: ClientAction) {
        let string = action.toJSONString()
        print("Sending : \(string)")
        socket.write(string: string)
    }

    func received(_ action: ServerAction) {
        switch action {
        case .registered(_):
            print("Successfully registered!")
        }
    }

    internal let socket : WebSocket
    internal let url = URL(string: "ws://localhost:8000")!

    func websocketDidConnect(socket: WebSocket) {
        print("Websocket connected")
        send(.register)
    }

    func websocketDidReceiveData(socket: WebSocket, data: Data) {
    }

    func websocketDidDisconnect(socket: WebSocket, error: NSError?) {
        assertionFailure(error!.localizedDescription)
    }

    func websocketDidReceiveMessage(socket: WebSocket, text: String) {
        guard let data = text.data(using: .utf8) else {
            return
        }

        switch ServerAction.fromJSON(data) {
        case .success(let action):
            received(action)
        case .error(let error):
            print(error)
        }
    }
}


