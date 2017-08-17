//
//  WebSocket.swift
//  Goojle Retain
//
//  Created by Timothy Barraclough on 17/08/17.
//  Copyright © 2017 Shop Share Industries. All rights reserved.
//

import Foundation
import Starscream

class GRWebSocket : WebSocketDelegate {

    static let shared = GRWebSocket()

    fileprivate init() {
        socket = WebSocket(url: url)
        socket.delegate = self
        socket.connect()

    }

    func send(_ action: ClientAction) {
        let string = action.toString()
        print("Sending : \(string)")
        socket.write(string: string)
    }

    internal let socket : WebSocket
    internal let url = URL(string: "ws://a77baf75.ngrok.io/")!

    func websocketDidConnect(socket: WebSocket) {
        print("Websocket connected")
        GRWebSocket.shared.send(.register)
    }

    func websocketDidReceiveData(socket: WebSocket, data: Data) {
    }

    func websocketDidDisconnect(socket: WebSocket, error: NSError?) {
        assertionFailure(error!.localizedDescription)
    }

    func websocketDidReceiveMessage(socket: WebSocket, text: String) {
        print(text)
    }

}
