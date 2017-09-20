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

    var observer: ListObserverProtocol? = nil {
        didSet {
            print("Set observer")
        }
    }

    lazy var delegate : GRWebSocketDelegateProtocol = self

    internal let socket : WebSocket
    internal let url = URL(string: "ws://180a9135.ngrok.io")!

    fileprivate init() {
        socket = WebSocket(url: url)
        socket.delegate = self
        socket.connect()
    }

    func websocketDidConnect(socket: WebSocket) {
        delegate.connected()
    }

    func websocketDidReceiveData(socket: WebSocket, data: Data) {
        delegate.receivedError(.incorrectDataFormatError)
    }

    func websocketDidDisconnect(socket: WebSocket, error: NSError?) {
        delegate.receivedError(.prematureDisconnection)
    }

    func websocketDidReceiveMessage(socket: WebSocket, text: String) {
        guard let data = text.data(using: .utf8) else {
            delegate.receivedError(.textToDataError)
            return
        }

        switch ServerAction.fromJSON(data) {
        case .success(let action):
            delegate.received(action)
        case .error(let error):
            delegate.receivedError(error)
        }
    }
}


